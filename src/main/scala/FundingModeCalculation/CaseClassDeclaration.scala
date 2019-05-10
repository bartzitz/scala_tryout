package FundingModeCalculation

object CaseClassDeclaration {
  case class AccountClassification(classification: AccountClassificationResponse) {
    def isClient: Boolean      = classification.compliance_relationship == "client"
    def isNotClient: Boolean   = classification.compliance_relationship == "non-client"
    def isRegulated: Boolean   = classification.regulated_service == "regulated"
    def isUnregulated: Boolean = classification.regulated_service == "unregulated"
  }

  case class AccountResolver(account: AccountClassification, houseAccount: Option[AccountClassification], sender: Sender) {
    def isNotComplianceRelationship: Boolean = {
      if (houseAccount.isEmpty) false else account.isNotClient && houseAccount.get.isNotClient
    }

    def isNestedPaymentsWithCollections: Boolean = {
      val isRegulated = if (houseAccount.isEmpty) account.isRegulated else houseAccount.get.isRegulated

      isRegulated && account.isClient && (sender.isNotAccountHolder || sender.isApprovedFundingPartner)
    }

    def isRegulatedAffiliateReceipts: Boolean = {
      if (houseAccount.isEmpty) false
      else account.isClient && houseAccount.get.isNotClient && houseAccount.get.isRegulated && sender.isAccountHolder
    }

    def isCorporateCollections: Boolean = {
      if (houseAccount.isEmpty) false
      else account.isNotClient && houseAccount.get.isClient && houseAccount.get.isUnregulated && sender.isAccountHolder
    }

    def isNestedCollections: Boolean = {
      if (houseAccount.isEmpty) false
      else account.isNotClient && houseAccount.get.isClient && houseAccount.get.isRegulated && (sender.isNotAccountHolder || sender.isApprovedFundingPartner)
    }
  }

  case class Sender(isApprovedFundingPartner: Boolean, isNotAccountHolder: Boolean, isAccountHolder: Boolean)
  case class Transaction(sender: Sender)
}
