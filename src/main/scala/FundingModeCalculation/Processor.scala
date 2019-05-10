package FundingModeCalculation

import scala.collection.mutable.ListBuffer
import FundingModeCalculation.Calculation._
import FundingModeCalculation.CaseClassDeclaration._
import net.liftweb.json.DefaultFormats
import net.liftweb.json._

case class AccountClassificationResponse(compliance_relationship: String, regulated_service: String)

object Processor extends App {
  /*
    This method should return account classification and house account classification
    As the Akka.http is not implemented (YET) I hardcoded the result
  */
  private def fetch_account_classification = {
    implicit val formats = DefaultFormats

    /*
      There are two possible responses
      1) {
         "account_classification": [
           { "compliance_relationship": "non-client", "regulated_service": "regulated" }  -> account classification
         ]
       }

      2)
      {
        "account_classification": [
          { "compliance_relationship": "non-client", "regulated_service": "regulated" }, -> account classification
          { "compliance_relationship": "non-client", "regulated_service": "regulated" }  -> house account classification
        ]
      }


    */
    val jsonString = """
      {
        "account_classification": [
          { "compliance_relationship": "non-client", "regulated_service": "regulated" }
        ]
      }
      """

    val json = parse(jsonString)
    val elements = (json \ "account_classification").children
    val classificationsList = new ListBuffer[AccountClassificationResponse]

    for (acct <- elements) { classificationsList += acct.extract[AccountClassificationResponse] }

    val bufferLength = classificationsList.length

    bufferLength match {
      case 1 => (AccountClassification(classificationsList(0)), None)
      case _ => (AccountClassification(classificationsList(0)), Some(AccountClassification(classificationsList(1))))
    }
  }

  def classifyFundingMode(transaction: Transaction) = {
    val sender = transaction.sender

    try {
      val (accountClassification, houseAccountClassification) = fetch_account_classification
      val resolver = AccountResolver(accountClassification, houseAccountClassification, sender)
      val fundingType = identifyFundingType(resolver, sender)

      val fundingMode = fundingType match {
        case Some(Collections) | Some(Receipts) => identifyFundingMode(fundingType, accountClassification, resolver)
        case _ => None
      }


      (fundingType, fundingMode)
    } catch {
      case e: Exception => println(e.getMessage) // log Exception
    }
  }

  val sender = Sender(true, true, true)
  val transaction = Transaction(sender)
  val (fundingType, fundingMode) = classifyFundingMode(transaction)

  println(s"Funding type -> $fundingType, Funding mode -> $fundingMode")
}
