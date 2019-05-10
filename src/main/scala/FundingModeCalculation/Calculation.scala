package FundingModeCalculation

import FundingModeCalculation.CaseClassDeclaration._

sealed trait FundingType
case object Prohibited extends FundingType
case object Collections extends FundingType
case object Receipts extends FundingType

sealed trait FundingMode
case object ReceiptsFromClient extends FundingMode
case object ReceiptsOboClient extends FundingMode
case object CollectionsOboClient extends FundingMode
case object CollectionsOboClientsCustomer extends FundingMode

object Calculation extends FundingType with FundingMode {
  def identifyFundingType(resolver: AccountResolver, sender: Sender): Option[FundingType] = {
    (resolver, sender) match {
      case (r: AccountResolver, _) if r.isNotComplianceRelationship || r.isNestedPaymentsWithCollections || r.isRegulatedAffiliateReceipts => Some(Prohibited)
      case (r: AccountResolver, s: Sender) if r.isCorporateCollections || s.isNotAccountHolder || s.isApprovedFundingPartner => Some(Collections)
      case (_, s: Sender) if s.isAccountHolder => Some(Receipts)
      case _ => None
    }
  }

  def identifyFundingMode(fundingType: Option[FundingType], account: AccountClassification, resolver: AccountResolver): Option[FundingMode] = {
    fundingType match {
      case Some(Receipts) => if (account.isClient) Some(ReceiptsFromClient) else Some(ReceiptsOboClient)
      case Some(Collections) => if (resolver.isNestedCollections) Some(CollectionsOboClientsCustomer) else Some(CollectionsOboClient)
      case _ => None
    }
  }
}
