package controllers

import model.TierPlan.FriendTierPlan

import scala.concurrent.Future

import play.api.mvc.Controller
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json

import com.gu.membership.salesforce.{FreeMember, PaidMember, Member, Tier}
import com.gu.membership.salesforce.Tier.Tier
import model.{Zuora, Stripe, PrivateFields}

import forms.MemberForm._
import model.StripeSerializer._
import services._

trait DowngradeTier {
  self: TierController =>

  def downgradeToFriend() = PaidMemberAction { implicit request =>
    Ok(views.html.tier.downgrade.confirm(request.member.tier))
  }

  def downgradeToFriendConfirm() = PaidMemberAction.async { implicit request => // POST
    for {
      cancelledSubscription <- MemberService.downgradeSubscription(request.member, FriendTierPlan)
    } yield Redirect("/tier/change/friend/summary")
  }

  def downgradeToFriendSummary() = PaidMemberAction.async { implicit request =>
    for {
      subscriptionStatus <- SubscriptionService.getSubscriptionStatus(request.member.salesforceAccountId)
      currentSubscription <- SubscriptionService.getSubscriptionDetails(subscriptionStatus.current)
      futureSubscription <- SubscriptionService.getSubscriptionDetails(subscriptionStatus.future.get)
    } yield Ok(views.html.tier.downgrade.summary(currentSubscription, futureSubscription))
  }
}

trait UpgradeTier {
  self: TierController =>

  def upgrade(tier: Tier) = MemberAction.async { implicit request =>
    if (request.member.tier < tier) {
      val identityRequest = IdentityRequest(request)
      for {
        userOpt <- IdentityService.getFullUserDetails(request.user, identityRequest)
        privateFields = userOpt.map(_.privateFields).getOrElse(PrivateFields.apply())
      } yield Ok(views.html.tier.upgrade.upgradeForm(tier, privateFields))
    }
    else
      Future.successful(NotFound)
  }

  def upgradeConfirm(tier: Tier) = MemberAction.async { implicit request =>
    val futureConfirm = request.member match {
      case freeMember: FreeMember =>
        paidMemberChangeForm.bindFromRequest.fold(_ => Future.successful(BadRequest), formData => {
          MemberService.upgradeSubscription(freeMember, request.user, tier, formData, IdentityRequest(request)).map(_ => Ok(""))
        })
      case _ => Future.successful(NotFound)
    }

    futureConfirm.recover {
      case error: Stripe.Error => Forbidden(Json.toJson(error))
      case error: Zuora.ResultError => Forbidden
    }
  }
}

trait CancelTier {
  self: TierController =>

  def cancelTier() = MemberAction { implicit request =>
    Ok(views.html.tier.cancel.confirm(request.member.tier))
  }

  def cancelTierConfirm() = MemberAction.async { implicit request =>
    for {
      _ <- MemberService.cancelSubscription(request.member)
    } yield {
      Redirect("/tier/cancel/summary")
    }
  }

  def cancelTierSummary() = AuthenticatedAction.async { implicit request =>
    def subscriptionDetailsFor(memberOpt: Option[Member]) = {
      memberOpt.collect { case paidMember: PaidMember =>
        SubscriptionService.getCurrentSubscriptionDetails(paidMember.salesforceAccountId)
      }
    }

    for {
      memberOpt <- MemberRepository.get(request.user.id)
      subscriptionDetails <- Future.sequence(subscriptionDetailsFor(memberOpt).toSeq)
    } yield Ok(views.html.tier.cancel.summary(subscriptionDetails.headOption))
  }
}

trait TierController extends Controller with UpgradeTier with DowngradeTier with CancelTier {

  def change() = MemberAction { implicit request =>
    Ok(views.html.tier.change(request.member.tier))
  }
}

object TierController extends TierController
