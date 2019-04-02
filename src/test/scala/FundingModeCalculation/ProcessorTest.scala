package FundingModeCalculation

import FundingModeCalculation.CaseClassDeclaration._
import org.scalatest.FunSpec
import org.scalamock.scalatest.MockFactory

class ProcessorTest extends FunSpec with MockFactory {
  describe(".classifyFundingMode") {
    describe("funding type is not prohibited") {
      val sender      = Sender(true, true, true)
      val transaction = Transaction(sender)

      it ("should returns prohibited") {
        assert(Processor.classifyFundingMode(transaction) == (Some(Collections), Some(CollectionsOboClientsCustomer)))
      }
    }
  }
}
