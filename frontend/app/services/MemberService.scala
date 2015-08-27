package services

import com.gu.identity.play.{IdMinimalUser, IdUser}
import com.gu.membership.model._
import com.gu.membership.salesforce.Member.Keys
import com.gu.membership.salesforce._
import com.gu.membership.stripe.Stripe
import com.gu.membership.stripe.Stripe.Customer
import com.gu.membership.util.Timing
import com.typesafe.scalalogging.LazyLogging
import configuration.Config
import controllers.IdentityRequest
import forms.MemberForm._
import model.Benefits.DiscountTicketTiers
import model.Eventbrite.{EBAccessCode, EBCode, EBOrder, EBTicketClass}
import model.{FeatureChoice, FreeEventTickets}
import model.RichEvent._
import model.Zuora.{CreateResult, PreviewInvoiceItem}
import model.ZuoraDeserializer.createResultReader
import model.Eventbrite.{EBCode, EBOrder, EBTicketClass}
import model.RichEvent._
import model.Zuora.{CreateResult, PreviewInvoiceItem}
import monitoring.MemberMetrics
import org.joda.time.Period
import play.api.libs.json.Json
import services.EventbriteService._
import tracking._
import utils.ScheduledTask
import zuora.CreateFreeEventUsage

import utils.TestUsers.isTestUser
import zuora.CreateFreeEventUsage
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

case class MemberServiceError(s: String) extends Throwable {
  override def getMessage: String = s
}

class FrontendMemberRepository(salesforceConfig: SalesforceConfig) extends MemberRepository {
  val metrics = new MemberMetrics(salesforceConfig.envName)

  val salesforce = new Scalaforce {
    val consumerKey = salesforceConfig.consumerKey
    val consumerSecret = salesforceConfig.consumerSecret

    val apiURL = salesforceConfig.apiURL.toString
    val apiUsername = salesforceConfig.apiUsername
    val apiPassword = salesforceConfig.apiPassword
    val apiToken = salesforceConfig.apiToken

    val stage = Config.stage
    val application = "Frontend"

    val authTask = ScheduledTask("", Authentication("", ""), 0.seconds, 30.minutes)(getAuthentication)

    def authentication: Authentication = authTask.get()
  }
}

trait MemberService extends LazyLogging with ActivityTracking {

  def initialData(user: IdUser, formData: JoinForm) = {
    Seq(Json.obj(
      Keys.EMAIL -> user.primaryEmailAddress,
      Keys.FIRST_NAME -> formData.name.first,
      Keys.LAST_NAME -> formData.name.last,
      Keys.MAILING_STREET -> formData.deliveryAddress.line,
      Keys.MAILING_CITY -> formData.deliveryAddress.town,
      Keys.MAILING_STATE -> formData.deliveryAddress.countyOrState,
      Keys.MAILING_POSTCODE -> formData.deliveryAddress.postCode,
      Keys.MAILING_COUNTRY -> formData.deliveryAddress.country.alpha2,
      Keys.ALLOW_MEMBERSHIP_MAIL -> true
    )) ++ Map(
      Keys.ALLOW_THIRD_PARTY_EMAIL -> formData.marketingChoices.thirdParty,
      Keys.ALLOW_GU_RELATED_MAIL -> formData.marketingChoices.gnm
    ).collect { case (k, Some(v)) => Json.obj(k -> v) }
  }.reduce(_ ++ _)

  def memberData(plan: ProductRatePlan, customerOpt: Option[Stripe.Customer]) = Json.obj(
    Keys.TIER -> plan.salesforceTier
  ) ++ customerOpt.map { customer =>
    Json.obj(
      Keys.CUSTOMER_ID -> customer.id,
      Keys.DEFAULT_CARD_ID -> customer.card.id
    )
  }.getOrElse(Json.obj())

  def createMember(user: IdMinimalUser, formData: JoinForm, identityRequest: IdentityRequest,
                   paymentDelay: Option[Period], campaignCode: Option[String] = None): Future[MemberId] = {
    val touchpointBackend = TouchpointBackend.forUser(user)
    val identityService = IdentityService(IdentityApi)

    Timing.record(touchpointBackend.memberRepository.metrics, "createMember") {
      def futureCustomerOpt = formData match {
        case paid: PaidMemberJoinForm => touchpointBackend.stripeService.Customer.create(user.id, paid.payment.token).map(Some(_))
        case _ => Future.successful(None)
      }

      formData.password.map(identityService.updateUserPassword(_, identityRequest, user.id))

      val casId = formData match {
        case paidMemberJoinForm: PaidMemberJoinForm => paidMemberJoinForm.casId
        case _ => None
      }

      for {
        fullUser <- identityService.getFullUserDetails(user, identityRequest)
        customerOpt <- futureCustomerOpt
        userData = initialData(fullUser, formData)
        memberId <- touchpointBackend.memberRepository.upsert(user.id, userData)
        subscription <- touchpointBackend.subscriptionService.createSubscription(memberId, formData, customerOpt, paymentDelay, casId)

        // Set some fields once subscription has been successful
        updatedMember <- touchpointBackend.memberRepository.upsert(user.id, memberData(formData.plan, customerOpt))
      } yield {
        identityService.updateUserFieldsBasedOnJoining(user, formData, identityRequest)

        touchpointBackend.memberRepository.metrics.putSignUp(formData.plan)
        trackRegistration(formData, memberId, user, campaignCode)
        memberId
      }
    }.andThen {
      case Success(memberAccount) => logger.debug(s"createMember() success user=${user.id} memberAccount=$memberAccount")
      case Failure(error) => {
        logger.error(s"Error in createMember() user=${user.id}", error)
        touchpointBackend.memberRepository.metrics.putFailSignUp(formData.plan)
      }
    }
  }

  def countComplimentaryTicketsInOrder(event: RichEvent, order: EBOrder): Int = {
    val ticketIds = event.internalTicketing.map(_.complimentaryTickets).getOrElse(Nil).map(_.id)
    order.attendees.count(attendee => ticketIds.contains(attendee.ticket_class_id))
  }

  def recordFreeEventUsage(member: Member, event: RichEvent, order: EBOrder, quantity: Int): Future[CreateResult] = {
    val tp = TouchpointBackend.forUser(member)
    for {
      account <- tp.subscriptionService.getAccount(member)
      subscriptions <- tp.subscriptionService.getSubscriptions(member)
      description = s"event-id:${event.id};order-id:${order.id}"
      action = CreateFreeEventUsage(account.id, description, quantity, subscriptions.head.id)
      result <- tp.zuoraSoapService.authenticatedRequest(action)
    } yield result
  }


  def retrieveComplimentaryTickets(member: Member, event: RichEvent): Future[Seq[EBTicketClass]] = {
    val tp = TouchpointBackend.forUser(member)
    val memberTierFeatures = tp.subscriptionService.memberTierFeatures(member)
    val ticketsUsed = tp.subscriptionService.getUsageCountWithinTerm(member, FreeEventTickets.unitOfMeasure)

    for {
      features <- memberTierFeatures
      usageCount <- ticketsUsed
    } yield {
      val hasComplimentaryTickets = features.map(_.featureCode).contains(FreeEventTickets.zuoraCode)
      val allowanceNotExceeded = usageCount < FreeEventTickets.allowance

      if (hasComplimentaryTickets && allowanceNotExceeded)
        event.internalTicketing.map(_.complimentaryTickets).getOrElse(Nil)
      else Nil
    }
  }

  def retrieveDiscountedTickets(member: Member, event: RichEvent): Seq[EBTicketClass] = {
    (for {
      ticketing <- event.internalTicketing
      benefit <- ticketing.memberDiscountOpt if DiscountTicketTiers.contains(member.tier)
    } yield ticketing.memberBenefitTickets)
      .getOrElse(Seq[EBTicketClass]())
  }

  def createEBCode(member: Member, event: RichEvent): Future[Option[EBCode]] = {
    val complimentaryTicketF =
      if (isTestUser(member)) retrieveComplimentaryTickets(member, event)
      else {
        Future.successful(Nil)
      }

    complimentaryTicketF.flatMap { complimentaryTickets =>
      val code = DiscountCode.generate(s"A_${member.identityId}_${event.id}")
      val unlockedTickets = complimentaryTickets ++ retrieveDiscountedTickets(member, event)
      event.service.createOrGetAccessCode(event, code, unlockedTickets)
    }
  }

  def previewUpgradeSubscription(paidMember: PaidMember, user: IdMinimalUser, newTier: Tier): Future[Seq[PreviewInvoiceItem]] = {
    val touchpointBackend = TouchpointBackend.forUser(user)

    for {
      paymentSummary <- touchpointBackend.subscriptionService.getPaymentSummary(paidMember)
      newRatePlan = PaidTierPlan(newTier, paymentSummary.current.annual)
      subscriptionResult <- touchpointBackend.subscriptionService.upgradeSubscription(paidMember, newRatePlan, preview = true, Set.empty)
    } yield subscriptionResult.invoiceItems
  }

  def upgradeFreeSubscription(freeMember: FreeMember, user: IdMinimalUser, newTier: Tier, form: FreeMemberChangeForm,
                              identityRequest: IdentityRequest, campaignCode: Option[String], featureChoice: Set[FeatureChoice]): Future[MemberId] = {
    val touchpointBackend = TouchpointBackend.forUser(user)

    for {
      customer <- touchpointBackend.stripeService.Customer.create(user.id, form.payment.token)
      paymentResult <- touchpointBackend.subscriptionService.createPaymentMethod(freeMember, customer)
      memberId <- upgradeSubscription(freeMember, user, newTier, Some(form), form.payment.annual, Some(customer), identityRequest, campaignCode, featureChoice)
    } yield memberId
  }

  def upgradePaidSubscription(paidMember: PaidMember, user: IdMinimalUser, newTier: Tier,
                              identityRequest: IdentityRequest, campaignCode: Option[String], featureChoice: Set[FeatureChoice]): Future[MemberId] = {
    for {
      paymentSummary <- TouchpointBackend.forUser(user).subscriptionService.getPaymentSummary(paidMember)
      memberId <- upgradeSubscription(paidMember, user, newTier, None, paymentSummary.current.annual, None, identityRequest, campaignCode, featureChoice)
    } yield memberId

  }

  private def upgradeSubscription(member: Member, user: IdMinimalUser, newTier: Tier, form: Option[FreeMemberChangeForm],
                                  annual: Boolean, customerOpt: Option[Customer], identityRequest: IdentityRequest,
                                   campaignCode: Option[String], featureChoice: Set[FeatureChoice]): Future[MemberId] = {
    val touchpointBackend = TouchpointBackend.forUser(user)
    val newRatePlan = PaidTierPlan(newTier, annual)

    for {
      subscriptionResult <- touchpointBackend.subscriptionService.upgradeSubscription(member, newRatePlan, preview = false, featureChoice)
      memberId <- touchpointBackend.memberRepository.upsert(member.identityId, memberData(newRatePlan, customerOpt))
    } yield {
      form.foreach(IdentityService(IdentityApi).updateUserFieldsBasedOnUpgrade(user, _, identityRequest))
      touchpointBackend.memberRepository.metrics.putUpgrade(newTier)
      track(
        MemberActivity(
          source = "membershipUpgrade",
          MemberData(
            salesforceContactId = memberId.salesforceContactId,
            identityId = user.id,
            tier = member.tier.name,
            tierAmendment = Some(UpgradeAmendment(member.tier, newTier)),
            deliveryPostcode = form.map(_.deliveryAddress.postCode),
            billingPostcode = form.flatMap(f => f.billingAddress.map(_.postCode)).orElse(form.map(_.deliveryAddress.postCode)),
            subscriptionPaymentAnnual = Some(annual),
            marketingChoices = None,
            city = form.map(_.deliveryAddress.town),
            country = form.map(_.deliveryAddress.country.name),
            campaignCode=campaignCode
          )
        ))(user)
      memberId
    }
  }

  private def trackRegistration(formData: JoinForm, member: MemberId, user: IdMinimalUser, campaignCode: Option[String] = None) {
    val subscriptionPaymentAnnual = formData match {
      case paidMemberJoinForm: PaidMemberJoinForm => Some(paidMemberJoinForm.payment.annual)
      case _ => None
    }

    val billingPostcode = formData match {
      case paidMemberJoinForm: PaidMemberJoinForm => paidMemberJoinForm.billingAddress.map(_.postCode).orElse(Some(formData.deliveryAddress.postCode))
      case _ => None
    }

    val trackingInfo =
      MemberData(
        member.salesforceContactId,
        user.id,
        formData.plan.salesforceTier,
        None,
        Some(formData.deliveryAddress.postCode),
        billingPostcode,
        subscriptionPaymentAnnual,
        Some(formData.marketingChoices),
        Some(formData.deliveryAddress.town),
        Some(formData.deliveryAddress.country.name),
        campaignCode
    )

    track(MemberActivity("membershipRegistration", trackingInfo), user)
  }
}

object MemberService extends MemberService
