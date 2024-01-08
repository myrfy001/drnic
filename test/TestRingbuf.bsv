import Vector :: *;
import UserLogicSettings :: *;
import UserLogicTypes :: *;
import DataTypes :: *;
import Headers :: *;
import FIFOF :: *;
import Arbitration :: *;
import PAClib :: *;
import PrimUtils :: *;
import ClientServer :: *;
import GetPut :: *;
import ConfigReg :: * ;
import Randomizable :: *;
import PrimUtils :: *;
import Utils :: *;
import Ringbuf :: *;

(* doc = "testcase" *)
module mkTestRingbuf(Empty) ;
    RingbufPool#(1,1, RingbufRawDescriptor) pool <- mkRingbufPool;

    Reg#(UInt#(20)) cntReg <- mkReg(1);
    Reg#(UInt#(3)) respCntReg <- mkReg(0);
    FIFOF#(UserLogicDmaH2cReq) pipelineH2CFifo<- mkFIFOF;
    FIFOF#(UserLogicDmaC2hReq) pipelineC2HFifo<- mkFIFOF;
    Reg#(UInt#(7)) expectH2cRecvReg <- mkReg(0); 
    Reg#(UInt#(7)) expectC2hRecvReg <- mkReg(0); 
    Randomize#(Bit#(10)) randomGen1 <- mkGenericRandomizer;
    Randomize#(Bit#(10)) randomGen2 <- mkGenericRandomizer;
    Randomize#(Bit#(10)) randomGen3 <- mkGenericRandomizer;
    Randomize#(Bit#(10)) randomGen4 <- mkGenericRandomizer;

    Reg#(UInt#(32)) softwareMoveHeadCntReg <- mkReg(0);
    Reg#(UInt#(32)) hardwareMoveHeadCntReg <- mkReg(0);
    Reg#(UInt#(32)) dmaDelaySimulateReg <- mkReg(0);

    Reg#(Bool) initializedReg <- mkReg(False);

    Reg#(UInt#(32)) testReg <- mkReg(0);
    FIFOF#(UInt#(32)) testFifo <- mkSizedFIFOF(3);

    rule testR;
        testFifo.enq(testReg);
        testReg <= testReg + 1;
    endrule

    rule testR1;
        testFifo.deq;
    endrule

    rule init if (!initializedReg);
        randomGen1.cntrl.init;
        randomGen2.cntrl.init;
        randomGen3.cntrl.init;
        randomGen4.cntrl.init;
        initializedReg <= True;
    endrule

    rule stop;
        cntReg <= cntReg + 1;
    endrule

    rule ruleSofrwareModifyH2cHead;
        let random <- randomGen1.next;
        if (isRingbufNotFull(pool.h2cMetas[0].head, pool.h2cMetas[0].tail)) begin
            
            
            // make a two stage random, first one the qeueu is almost empty, second one the queue is almost full
            if (
                (softwareMoveHeadCntReg < 20000 && random < 50) || 
                (softwareMoveHeadCntReg >= 20000 && random > 500)
            ) begin
                softwareMoveHeadCntReg <= softwareMoveHeadCntReg + 1;
                pool.h2cMetas[0].head <= pool.h2cMetas[0].head + 1;
            end

            if (softwareMoveHeadCntReg > 40000) begin
                $display("Passed");
                $finish();
            end
        end
    endrule 

    rule ruleFakeUserLogicWriteC2hHead;
        let random <- randomGen2.next; 
        // make a two stage random, first one the qeueu is almost empty, second one the queue is almost full
  
        if (
            (hardwareMoveHeadCntReg < 20000 && random < 50) || 
            (hardwareMoveHeadCntReg >= 20000 && random > 500)
        ) begin
            hardwareMoveHeadCntReg <= hardwareMoveHeadCntReg + 1;
            pool.c2hRings[0].enq(zeroExtend(pack(hardwareMoveHeadCntReg)));
        end
    endrule

    rule ruleGetToFifoRelayH2c;
        let req <- pool.dmaAccessH2cClt.request.get;
        pipelineH2CFifo.enq(req);
    endrule

    rule ruleGetToFifoRelayC2h;
        let req <- pool.dmaAccessC2hClt.request.get;
        pipelineC2HFifo.enq(req);
    endrule

    rule ruleFakeDmaEngineGeneratingH2CResponse;
        let random <- randomGen3.next;
        if (dmaDelaySimulateReg > 0) begin
            dmaDelaySimulateReg <= dmaDelaySimulateReg -1;
        end
        if (pipelineH2CFifo.notEmpty && dmaDelaySimulateReg == 0) begin
            if ((respCntReg & 'h7) == 7) begin
                pipelineH2CFifo.deq;
                dmaDelaySimulateReg <= unpack(zeroExtend(random & 'h1F));
            end

            UInt#(32) reqAddr = unpack(truncate(pipelineH2CFifo.first.addr));

            UserLogicDmaH2cResp resp = unpack(0);
            respCntReg <= respCntReg + 1;
            
            resp.dataStream.isFirst = (respCntReg & 'h7) == 0;
            resp.dataStream.isLast = (respCntReg & 'h7) == 7;
            resp.dataStream.data = extend(pack(reqAddr) + extend(pack(respCntReg)) * 32);

            pool.dmaAccessH2cClt.response.put(resp);
        end
    endrule

    rule ruleFakeDmaEngineGeneratingC2HResponse;
        if (pipelineC2HFifo.notEmpty) begin
            expectC2hRecvReg <= expectC2hRecvReg + 1;

            let expectNumber = expectC2hRecvReg;
            UInt#(7) descNumber = unpack(truncate(pipelineC2HFifo.first.dataStream.data));
            immAssert(
                expectNumber == descNumber,
                "c2h descriptor write error @ mkTestRingbuf",
                $format(
                    "expectRecv=%h should == received=%h, ",
                    expectNumber, descNumber
                )
            );

            pipelineC2HFifo.deq;
            UserLogicDmaC2hResp resp = unpack(0);
            pool.dmaAccessC2hClt.response.put(resp);
        end
    endrule

    rule fakeUserLogicReadFifo;
        let descriptor = pool.h2cRings[0].first;
        pool.h2cRings[0].deq;

        Bit#(32) descNumber = truncate(descriptor);
        Bit#(32) expectNumber = extend(pack(expectH2cRecvReg)) * 32;
        $display(fshow(descriptor), expectH2cRecvReg, descNumber);
        immAssert(
            expectNumber == descNumber,
            "h2c descriptor read error @ mkTestRingbuf",
            $format(
                "expectRecv=%h should == received=%h, ",
                expectNumber, descNumber
            )
        );
        expectH2cRecvReg <= expectH2cRecvReg + 1;
    endrule

    rule fakeSoftwareMoveC2hTail;
        let random <- randomGen4.next;
        if (isRingbufNotEmpty(pool.c2hMetas[0].head, pool.c2hMetas[0].tail)) begin
            if (random < 50) begin 
                pool.c2hMetas[0].tail <= pool.c2hMetas[0].tail + 1;
            end
            else if (random < 100) begin
                pool.c2hMetas[0].tail <= pool.c2hMetas[0].head;
            end
        end
    endrule
endmodule




interface TestRingbufSynthesizeable;
    interface Vector#(1, PipeOut#(RingbufRawDescriptor)) h2cRings;

    interface Vector#(1, C2HRingBufFifoIfc#(RingbufRawDescriptor)) c2hRings;

endinterface

(* synthesize *)
module mkTestRingbufSynthesizeable(TestRingbufSynthesizeable) ;
    RingbufPool#(1,1, RingbufRawDescriptor) pool <- mkRingbufPool;

    interface h2cRings = pool.h2cRings;

    interface c2hRings = pool.c2hRings;

    
endmodule