package FundingModeCalculation

object CaseClassDeclaration {
  case class AccountClassification(classification: Map[String, String]) {
    def isClient: Boolean      = classification.getOrElse("compliance_relationship", null) == "client"
    def isNotClient: Boolean   = classification.getOrElse("compliance_relationship", null) == "non-client"
    def isRegulated: Boolean   = classification.getOrElse("regulated_service", null) == "regulated"
    def isUnregulated: Boolean = classification.getOrElse("regulated_service", null) == "unregulated"
  }

  case class AccountResolver(account: AccountClassification, houseAccount: AccountClassification = null, sender: Sender) {
    def isNotComplianceRelationship: Boolean = {
      if (houseAccount == null) account.isNotClient else account.isNotClient && houseAccount.isNotClient
    }

    def isNestedPaymentsWithCollections: Boolean = {
      val isRegulated = if (houseAccount == null) account.isRegulated else houseAccount.isRegulated

      isRegulated && account.isClient && (sender.isNotAccountHolder || sender.isApprovedFundingPartner)
    }

    def isRegulatedAffiliateReceipts: Boolean = {
      if (houseAccount == null) false
      else account.isClient && houseAccount.isNotClient && houseAccount.isRegulated && sender.isAccountHolder
    }

    def isCorporateCollections: Boolean = {
      if (houseAccount == null) false
      else account.isNotClient && houseAccount.isClient && houseAccount.isUnregulated && sender.isAccountHolder
    }

    def isNestedCollections: Boolean = {
      if (houseAccount == null) false
      else account.isNotClient && houseAccount.isClient && houseAccount.isRegulated && (sender.isNotAccountHolder || sender.isApprovedFundingPartner)
    }
  }

  case class Sender(isApprovedFundingPartner: Boolean, isNotAccountHolder: Boolean, isAccountHolder: Boolean)
  case class Transaction(sender: Sender)
}
