package FundingMode

import FundingMode.Calculation._
import FundingMode.CaseClassDeclaration._

object Processor extends App {
  private def fetch_account_classification = {
    /*
      This method should return account classification and house account classification
      As the Akka.http is not implemented (YET) I hardcoded the result
    */

    (
      AccountClassification(Map("compliance_relationship" -> "non-client", "regulated_service" -> "regulated")),
      AccountClassification(Map("compliance_relationship" -> "client", "regulated_service" -> "regulated"))
    )
  }

  def classifyFundingMode(transaction: Transaction) = {
    val sender = transaction.sender

    try {
      val (accountClassification, houseAccountClassification) = fetch_account_classification
      val resolver = Resolver(accountClassification, houseAccountClassification, sender)
      val fundingType = identifyFundingType(resolver, sender)
      val fundingMode = if (fundingType == prohibited) null else identifyFundingMode(fundingType, accountClassification, resolver)

      // Update transaction with fundingType, fundingMode

      (fundingType, fundingMode)
    } catch {
      case _: Exception => // log Exception
    }
  }

  val sender = Sender(true, true, true)
  val transaction = Transaction(sender)
  val (fundingType, fundingMode) = classifyFundingMode(transaction)

  println(s"Funding type -> $fundingType, Funding mode -> $fundingMode")
}
