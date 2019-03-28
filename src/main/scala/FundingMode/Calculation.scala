package FundingMode

import FundingMode.CaseClassDeclaration._

object Calculation {
  val prohibited  = "prohibited"
  val collections = "collections"
  val receipts    = "receipts"

  val from_client          = "from_client"
  val obo_client           = "obo_client"
  val obo_clients_customer = "obo_clients_customer"

  def identifyFundingType(resolver: Resolver, sender: Sender): String = {
    if (resolver.isNotComplianceRelationship || resolver.isNestedPaymentsWithCollections || resolver.isRegulatedAffiliateReceipts) prohibited
    else if (resolver.isCorporateCollections || sender.isNotAccountHolder || sender.isApprovedFundingPartner) collections
    else if (sender.isAccountHolder) receipts
    else throw new IllegalArgumentException("Can't resolve funding_type")
  }

  def identifyFundingMode(fundingType: String, account: AccountClassification, resolver: Resolver): String = {
    val fundingMode = fundingType match {
      case "receipts"    => if (account.isClient) from_client else obo_client
      case "collections" => if (resolver.isNestedCollections) obo_clients_customer else obo_client
      case _             => null
    }

    fundingType + "_" + fundingMode
  }
}
