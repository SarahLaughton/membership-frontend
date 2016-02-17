package controllers

import actions.RichAuthRequest
import com.gu.i18n.Country
import com.gu.i18n.CountryGroup._
import com.gu.memsub.BillingPeriod
import com.gu.memsub.promo.Writers._
import com.gu.memsub.promo._
import com.gu.salesforce.{FreeTier, PaidTier, Tier}
import model._
import play.api.libs.json._
import play.api.mvc.Controller
import services.TouchpointBackend
import views.support.PageInfo

import scalaz.syntax.std.option._

object Promotions extends Controller {

  val pageImages = Seq(
    ResponsiveImageGroup(
      name=Some("fearless"),
      metadata=Some(Grid.Metadata(
        description = Some("The Counted: people killed by police in the United States in 2015"),
        byline = Some("The Guardian US"),
        credit = None
      )),
      availableImages=ResponsiveImageGenerator(
        id="201ae0837f996f47b75395046bdbc30aea587443/0_0_1140_684",
        sizes=List(1000,500)
      )
    ),
    ResponsiveImageGroup(
      name=Some("stonehenge"),
      metadata=Some(Grid.Metadata(
        description = None,
        byline = None,
        credit = None
      )),
      availableImages=ResponsiveImageGenerator(
        id="5bb56c3a99704f4f2d03db5c304ee39415331747/184_0_6780_4071",
        sizes=List(1000,500)
      )
    )
  )

  def promotionPage(promoCodeStr: String) = GoogleAuthenticatedStaffAction { implicit request =>

    def findTemplateForPromotion(promoCode: PromoCode, promotion: Promotion, url: String) =
      promotion.promotionType match {
        case Incentive =>
          implicit val countryGroup = UK

          Some(views.html.promotions.englishHeritageOffer(
            TouchpointBackend.Normal.catalog.partner,
            PageInfo(
              title = promotion.title,
              url = url,
              description = Some(promotion.description)
            ),
            pageImages
          ))
        case _ => None
      }

    val promoCode = PromoCode(promoCodeStr)

    (for {
      promotion <- TouchpointBackend.Normal.promoService.findPromotion(promoCode)
      html <- if (promotion.expires.isBeforeNow) None else findTemplateForPromotion(promoCode, promotion, request.path)
    } yield Ok(html)).getOrElse(NotFound(views.html.error404()))

  }

  def validatePromoCode(promoCode: PromoCode, billingPeriod: BillingPeriod, tier: Tier, country: Country) = AuthenticatedAction { implicit request =>
    implicit val catalog = request.touchpointBackend.catalog

    val prpId = tier match {
      case t: PaidTier => PaidPlanChoice(t, billingPeriod).productRatePlanId
      case t: FreeTier => FreePlanChoice(t).productRatePlanId
    }

    val p = for {
      promo <- request.touchpointBackend.promoService.findPromotion(promoCode).toRightDisjunction(NoSuchCode)
      _ <- promo.validateFor(prpId, country)
    } yield promo

    p.fold(
      {err => BadRequest(Json.toJson(err))},
      {promo => Ok(Json.toJson(promo))}
    )
  }
}