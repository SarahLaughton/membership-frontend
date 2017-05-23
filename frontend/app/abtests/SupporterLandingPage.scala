package abtests

import abtests.AudienceRange.FullAudience

case object SupporterLandingPage extends ABTest("supporter-landing-page", FullAudience) {

  case class Variant(slug: String, showNewDesign: Boolean) extends BaseVariant

  val variants = Seq(
    Variant("control", showNewDesign = false),
    Variant("new-design",  showNewDesign = true)
  )
}