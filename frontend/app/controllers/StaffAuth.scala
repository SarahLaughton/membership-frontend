package controllers

import actions.{CommonActions, OAuthActions}
import com.gu.googleauth.GoogleAuthConfig
import model.FlashMessage
import play.api.libs.ws.WSClient
import play.api.mvc._

import scala.concurrent.ExecutionContext

class StaffAuth(
  override val wsClient: WSClient,
  parser: BodyParser[AnyContent],
  executionContext: ExecutionContext,
  googleAuthConfig: GoogleAuthConfig,
  commonActions: CommonActions,
  override protected val controllerComponents: ControllerComponents
) extends OAuthActions(parser, executionContext, googleAuthConfig, commonActions) with BaseController {

  def unauthorised = GoogleAuthenticatedStaffAction { implicit request =>
    val flashMsgOpt = request.flash.get("error").map(FlashMessage.error)
    Ok(views.html.staff.unauthorised(flashMsgOpt, request.flash.get("errorTemplate"), request.user.email))
  }
}
