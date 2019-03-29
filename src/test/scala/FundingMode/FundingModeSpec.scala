package FundingMode

import FundingMode.CaseClassDeclaration._
import org.scalatest._

class FundingModeSpec extends FunSpec {
  describe(".identifyFundingType set") {
    describe("not compliance relationship") {
      val sender       = Sender(false, false, false)
      val account      = AccountClassification(Map("compliance_relationship" -> "non-client", "regulated_service" -> ""))
      val houseAccount = AccountClassification(Map("compliance_relationship" -> "non-client", "regulated_service" -> ""))
      val resolver     = AccountResolver(account, houseAccount, sender)

      it ("should returns prohibited") {
        assert(Calculation.identifyFundingType(resolver, sender) == "prohibited")
      }
    }

    describe("nested payments with collections") {
      val sender       = Sender(false, true, false)
      val account      = AccountClassification(Map("compliance_relationship" -> "client", "regulated_service" -> ""))
      val houseAccount = AccountClassification(Map("compliance_relationship" -> "non-client", "regulated_service" -> "regulated"))
      val resolver     = AccountResolver(account, houseAccount, sender)

      it ("should returns prohibited") {
        assert(Calculation.identifyFundingType(resolver, sender) == "prohibited")
      }
    }

    describe("regulated affiliate receipts") {
      val sender       = Sender(false, false, true)
      val account      = AccountClassification(Map("compliance_relationship" -> "client", "regulated_service" -> ""))
      val houseAccount = AccountClassification(Map("compliance_relationship" -> "non-client", "regulated_service" -> "regulated"))
      val resolver     = AccountResolver(account, houseAccount, sender)

      it ("should returns prohibited") {
        assert(Calculation.identifyFundingType(resolver, sender) == "prohibited")
      }
    }

    describe("corporate collections") {
      val sender       = Sender(false, true, false)
      val account      = AccountClassification(Map("compliance_relationship" -> "non-client", "regulated_service" -> ""))
      val houseAccount = AccountClassification(Map("compliance_relationship" -> "client", "regulated_service" -> "regulated"))
      val resolver     = AccountResolver(account, houseAccount, sender)

      it ("should returns prohibited") {
        assert(Calculation.identifyFundingType(resolver, sender) == "collections")
      }
    }

    describe("sender is not account holder") {
      val sender       = Sender(false, true, false)
      val account      = AccountClassification(Map("compliance_relationship" -> "non-client", "regulated_service" -> ""))
      val houseAccount = AccountClassification(Map("compliance_relationship" -> "client", "regulated_service" -> "unregulated"))
      val resolver     = AccountResolver(account, houseAccount, sender)

      it ("should returns prohibited") {
        assert(Calculation.identifyFundingType(resolver, sender) == "collections")
      }
    }

    describe("sender is approved funding partner") {
      val sender       = Sender(true, false, false)
      val account      = AccountClassification(Map("compliance_relationship" -> "non-client", "regulated_service" -> ""))
      val houseAccount = AccountClassification(Map("compliance_relationship" -> "client", "regulated_service" -> "unregulated"))
      val resolver     = AccountResolver(account, houseAccount, sender)

      it ("should returns prohibited") {
        assert(Calculation.identifyFundingType(resolver, sender) == "collections")
      }
    }

    describe("sender is account holder") {
      val sender       = Sender(false, false, true)
      val account      = AccountClassification(Map("compliance_relationship" -> "client", "regulated_service" -> ""))
      val houseAccount = AccountClassification(Map("compliance_relationship" -> "client", "regulated_service" -> "unregulated"))
      val resolver     = AccountResolver(account, houseAccount, sender)

      it ("should returns prohibited") {
        assert(Calculation.identifyFundingType(resolver, sender) == "receipts")
      }
    }

    describe("no valid account configuration") {
      val sender       = Sender(false, false, false)
      val account      = AccountClassification(Map("compliance_relationship" -> "", "regulated_service" -> ""))
      val houseAccount = AccountClassification(Map("compliance_relationship" -> "", "regulated_service" -> ""))
      val resolver     = AccountResolver(account, houseAccount, sender)

      it ("produce IllegalArgumentException exception") {
        assertThrows[IllegalArgumentException] {
          Calculation.identifyFundingType(resolver, sender)
        }
      }
    }
  }

  describe(".identifyFundingType set") {
    describe("fundingType receipts") {
      describe("account is client") {
        val fundingType = "receipts"
        val sender = Sender(false, false, false)
        val account = AccountClassification(Map("compliance_relationship" -> "client", "regulated_service" -> ""))
        val houseAccount = AccountClassification(Map("compliance_relationship" -> "non-client", "regulated_service" -> ""))
        val resolver = AccountResolver(account, houseAccount, sender)

        it("should returns receipts_from_client") {
          assert(Calculation.identifyFundingMode(fundingType, account, resolver) == "receipts_from_client")
        }
      }

      describe("account is not client") {
        val fundingType = "receipts"
        val sender = Sender(false, false, false)
        val account = AccountClassification(Map("compliance_relationship" -> "non-client", "regulated_service" -> ""))
        val houseAccount = AccountClassification(Map("compliance_relationship" -> "non-client", "regulated_service" -> ""))
        val resolver = AccountResolver(account, houseAccount, sender)

        it("should returns receipts_obo_client") {
          assert(Calculation.identifyFundingMode(fundingType, account, resolver) == "receipts_obo_client")
        }
      }
    }

    describe("fundingType collections") {
      describe("account is nested collections") {
        val fundingType = "collections"
        val sender = Sender(false, true, false)
        val account = AccountClassification(Map("compliance_relationship" -> "non-client", "regulated_service" -> ""))
        val houseAccount = AccountClassification(Map("compliance_relationship" -> "client", "regulated_service" -> "regulated"))
        val resolver = AccountResolver(account, houseAccount, sender)

        it("should returns collections_obo_clients_customer") {
          assert(Calculation.identifyFundingMode(fundingType, account, resolver) == "collections_obo_clients_customer")
        }
      }

      describe("account is not client") {
        val fundingType = "collections"
        val sender = Sender(false, false, false)
        val account = AccountClassification(Map("compliance_relationship" -> "non-client", "regulated_service" -> ""))
        val houseAccount = AccountClassification(Map("compliance_relationship" -> "non-client", "regulated_service" -> ""))
        val resolver = AccountResolver(account, houseAccount, sender)

        it("should returns collections_obo_client") {
          assert(Calculation.identifyFundingMode(fundingType, account, resolver) == "collections_obo_client")
        }
      }
    }
  }
}
