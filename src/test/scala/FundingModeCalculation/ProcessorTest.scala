package FundingModeCalculation

import FundingModeCalculation.CaseClassDeclaration._
import org.scalatest.FunSpec

class ProcessorTest extends FunSpec {
  describe(".classifyFundingMode") {
    describe("funding type is prohibited") {
      val sender      = Sender(true, true, true)
      val transaction = Transaction(sender)

      it ("should returns prohibited") {
        assert(Processor.classifyFundingMode(transaction) == (Some(Collections), Some(CollectionsOboClient)))
      }
    }
  }
}
