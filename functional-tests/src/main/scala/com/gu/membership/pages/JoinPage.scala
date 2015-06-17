package com.gu.membership.pages

import org.openqa.selenium.{By, WebDriver}

class JoinPage(driver: WebDriver) extends BaseMembershipPage(driver) {

  private def becomeAFriendLink = driver.findElement(By.cssSelector(".qa-friend-join"))
  private def becomeAPartnerLink = driver.findElement(By.cssSelector(".qa-package-partner"))
  private def becomeAPatronLink = driver.findElement(By.cssSelector(".qa-package-patron"))

  def clickBecomeAFriend = {
    becomeAFriendLink.click
    new JoinFriendPage(driver)
  }

  def clickBecomeAPatron = {
    becomeAPatronLink.click
    new JoinPatronPage(driver)
  }

  def clickBecomeAPartner = {
    becomeAPartnerLink.click
    new JoinPartnerPage(driver)
  }

  def isPageLoaded = becomeAFriendLink.isDisplayed && becomeAPatronLink.isDisplayed
}
