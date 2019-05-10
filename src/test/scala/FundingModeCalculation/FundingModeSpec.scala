package FundingModeCalculation

import FundingModeCalculation.CaseClassDeclaration._
import org.scalatest._

class FundingModeSpec extends FunSpec {
  describe(".identifyFundingType set") {
    describe("not compliance relationship") {
      val sender       = Sender(false, false, false)
      val account      = AccountClassification(AccountClassificationResponse("non-client", "regulated"))
      val houseAccount = Some(AccountClassification(AccountClassificationResponse("non-client", "regulated")))
      val resolver     = AccountResolver(account, houseAccount, sender)

      it ("should returns prohibited") {
        assert(Calculation.identifyFundingType(resolver, sender).contains(Prohibited))
      }
    }

    describe("nested payments with collections") {
      val sender       = Sender(false, true, false)
      val account      = AccountClassification(AccountClassificationResponse("client", "regulated"))
      val houseAccount = Some(AccountClassification(AccountClassificationResponse("non-client", "regulated")))
      val resolver     = AccountResolver(account, houseAccount, sender)

      it ("should returns prohibited") {
        assert(Calculation.identifyFundingType(resolver, sender).contains(Prohibited))
      }
    }

    describe("regulated affiliate receipts") {
      val sender       = Sender(false, false, true)
      val account      = AccountClassification(AccountClassificationResponse("client", "regulated"))
      val houseAccount = Some(AccountClassification(AccountClassificationResponse("non-client", "regulated")))
      val resolver     = AccountResolver(account, houseAccount, sender)

      it ("should returns prohibited") {
        assert(Calculation.identifyFundingType(resolver, sender).contains(Prohibited))
      }
    }

    describe("corporate collections") {
      val sender       = Sender(false, true, false)
      val account      = AccountClassification(AccountClassificationResponse("non-client", "regulated"))
      val houseAccount = Some(AccountClassification(AccountClassificationResponse("client", "regulated")))
      val resolver     = AccountResolver(account, houseAccount, sender)

      it ("should returns collections") {
        assert(Calculation.identifyFundingType(resolver, sender).contains(Collections))
      }
    }

    describe("sender is not account holder") {
      val sender       = Sender(false, true, false)
      val account      = AccountClassification(AccountClassificationResponse("client", "regulated"))
      val houseAccount = Some(AccountClassification(AccountClassificationResponse("client", "unregulated")))
      val resolver     = AccountResolver(account, houseAccount, sender)

      it ("should returns collections") {
        assert(Calculation.identifyFundingType(resolver, sender).contains(Collections))
      }
    }

    describe("sender is approved funding partner") {
      val sender       = Sender(true, false, false)
      val account      = AccountClassification(AccountClassificationResponse("non-client", "regulated"))
      val houseAccount = Some(AccountClassification(AccountClassificationResponse("client", "unregulated")))
      val resolver     = AccountResolver(account, houseAccount, sender)

      it ("should returns collections") {
        assert(Calculation.identifyFundingType(resolver, sender).contains(Collections))
      }
    }

    describe("sender is account holder") {
      val sender       = Sender(false, false, true)
      val account      = AccountClassification(AccountClassificationResponse("client", "regulated"))
      val houseAccount = Some(AccountClassification(AccountClassificationResponse("client", "unregulated")))
      val resolver     = AccountResolver(account, houseAccount, sender)

      it ("should returns receipts") {
        assert(Calculation.identifyFundingType(resolver, sender).contains(Receipts))
      }
    }

    describe("no valid account configuration") {
      val sender       = Sender(false, false, false)
      val account      = AccountClassification(AccountClassificationResponse("", ""))
      val houseAccount = Some(AccountClassification(AccountClassificationResponse("", "")))
      val resolver     = AccountResolver(account, houseAccount, sender)

      it ("should returns None") {
        assert(Calculation.identifyFundingType(resolver, sender).isEmpty)
      }
    }
  }

  describe(".identifyFundingType set") {
    describe("fundingType receipts") {
      describe("account is client") {
        val fundingType = Some(Receipts)
        val sender = Sender(false, false, false)
        val account = AccountClassification(AccountClassificationResponse("client", "regulated"))
        val houseAccount = Some(AccountClassification(AccountClassificationResponse("non-client", "regulated")))
        val resolver = AccountResolver(account, houseAccount, sender)

        it("should returns receipts_from_client") {
          assert(Calculation.identifyFundingMode(fundingType, account, resolver).contains(ReceiptsFromClient))
        }
      }

      describe("account is not client") {
        val fundingType = Some(Receipts)
        val sender = Sender(false, false, false)
        val account = AccountClassification(AccountClassificationResponse("non-client", "regulated"))
        val houseAccount = Some(AccountClassification(AccountClassificationResponse("non-client", "")))
        val resolver = AccountResolver(account, houseAccount, sender)

        it("should returns receipts_obo_client") {
          assert(Calculation.identifyFundingMode(fundingType, account, resolver).contains(ReceiptsOboClient))
        }
      }
    }

    describe("fundingType collections") {
      describe("account is nested collections") {
        val fundingType = Some(Collections)
        val sender = Sender(false, true, false)
        val account = AccountClassification(AccountClassificationResponse("non-client", ""))
        val houseAccount = Some(AccountClassification(AccountClassificationResponse("client", "regulated")))
        val resolver = AccountResolver(account, houseAccount, sender)

        it("should returns collections_obo_clients_customer") {
          assert(Calculation.identifyFundingMode(fundingType, account, resolver).contains(CollectionsOboClientsCustomer))
        }
      }

      describe("account is not client") {
        val fundingType = Some(Collections)
        val sender = Sender(false, false, false)
        val account = AccountClassification(AccountClassificationResponse("client", ""))
        val houseAccount = Some(AccountClassification(AccountClassificationResponse("non-client", "")))
        val resolver = AccountResolver(account, houseAccount, sender)

        it("should returns collections_obo_client") {
          assert(Calculation.identifyFundingMode(fundingType, account, resolver).contains(CollectionsOboClient))
        }
      }
    }

    describe("no fundingType were resolved") {
      val fundingType = None
      val sender = Sender(false, false, false)
      val account = AccountClassification(AccountClassificationResponse("", ""))
      val houseAccount = Some(AccountClassification(AccountClassificationResponse("", "")))
      val resolver = AccountResolver(account, houseAccount, sender)

      it ("should returns None") {
        assert(Calculation.identifyFundingMode(fundingType, account, resolver).isEmpty)
      }
    }
  }
}
