import FIFOF :: *;
import ClientServer :: * ;
import GetPut :: *;
import Clocks :: * ;
import Vector :: *;
import BRAM :: *;

import UserLogicSettings :: *;
import UserLogicTypes :: *;

import DataTypes :: *;
import SemiFifo :: *;
import BusConversion :: *;
import AxiStreamTypes :: *;
import Axi4LiteTypes :: *;
import Headers :: *;
import RegisterBlock :: *;
import Gearbox :: *;
import AlignedFIFOs :: * ;

import PrimUtils :: *;
import Connectable :: * ;
import StmtFSM::*;
import Randomizable :: * ;

import XdmaWrapper :: *;


(* doc = "testcase" *)
module mkTestGearbox(Empty);
    ClockDividerIfc divClk <- mkClockDivider(2);
    Reset slowReset <- mkInitialReset(1, clocked_by divClk.slowClock);
    XdmaGearbox dut <- mkXdmaGearbox(divClk.slowClock, slowReset);

    FakeXdma fakeXdma <- mkFakeXdma(2, tagged None, clocked_by divClk.slowClock, reset_by slowReset);

    FIFOF#(UserLogicDmaH2cReq) userH2cReqQ <- mkFIFOF;
    FIFOF#(UserLogicDmaH2cResp) userH2cRespQ <- mkFIFOF;
    FIFOF#(UserLogicDmaC2hReq) userC2hReqQ <- mkFIFOF;
    FIFOF#(UserLogicDmaC2hResp) userC2hRespQ <- mkFIFOF;


    mkConnection(fakeXdma.xdmaH2cSrv, dut.h2cStreamClt);
    mkConnection(fakeXdma.xdmaC2hSrv, dut.c2hStreamClt);

    mkConnection(toGet(userH2cReqQ), dut.h2cStreamSrv.request);
    mkConnection(toPut(userH2cRespQ), dut.h2cStreamSrv.response);
    mkConnection(toGet(userC2hReqQ), dut.c2hStreamSrv.request);
    mkConnection(toPut(userC2hRespQ), dut.c2hStreamSrv.response);

    Reg#(Bool) startedXdma <- mkReg(False, clocked_by divClk.slowClock, reset_by slowReset);
    Reg#(Bool) startedUser <- mkReg(False);
    Reg#(Bit#(10)) counter <- mkReg(0);

    FSM runUserSideLogic <- mkFSM(
        (seq

            userC2hReqQ.enq(UserLogicDmaC2hReq{
                addr: 'h0,
                len: 2,
                dataStream: DataStream{
                    data: 'h12345678,
                    isFirst:True,
                    isLast: True,
                    byteEn: 'hF
                }
            });

            userC2hRespQ.deq;

            userC2hReqQ.enq(UserLogicDmaC2hReq{
                addr: 'h40,
                len: 'h40,
                dataStream: DataStream{
                    data: 'h1234,
                    isFirst:True,
                    isLast: False,
                    byteEn: -1
                }
            });

            userC2hReqQ.enq(UserLogicDmaC2hReq{
                addr: 'h40,
                len: 'h4,
                dataStream: DataStream{
                    data: 'habcd,
                    isFirst:False,
                    isLast: True,
                    byteEn: -1
                }
            });

            userC2hRespQ.deq;

            userC2hReqQ.enq(UserLogicDmaC2hReq{
                addr: 'h23,
                len: 'h2,
                dataStream: DataStream{
                    data: 'hBB,
                    isFirst:True,
                    isLast: True,
                    byteEn: 1
                }
            });
            userC2hRespQ.deq;

            delay(200);

            userH2cReqQ.enq(UserLogicDmaH2cReq{
                addr: 'h0,
                len: 128
            });
            userH2cRespQ.deq;
            userH2cRespQ.deq;

            $display("pass");
            $finish;

        endseq)
    );

    rule doTest if (!startedUser && counter > 10);
        startedUser <= True;
        runUserSideLogic.start;
    endrule

    rule doKeepRunning;
        counter <= counter + 1;
    endrule
    
endmodule


(* doc = "testcase" *)
module mkTestFakeXdma(Empty);

    FakeXdma fakeXdmaSrc <- mkFakeXdma(0, tagged None);
    FakeXdma fakeXdmaDst <- mkFakeXdma(1, tagged None);

    Reg#(UInt#(32)) i <- mkReg(0);
    Reg#(Bool) startedUser <- mkReg(False);

    FIFOF#(UserLogicDmaC2hResp) srcC2hRespQ <- mkFIFOF;
    mkConnection(toPut(srcC2hRespQ), fakeXdmaSrc.xdmaC2hSrv.response);

    Randomize#(Bit#(13)) startAddrRnd <- mkGenericRandomizer;
    Randomize#(Bit#(8)) lenRnd <- mkGenericRandomizer;
    FIFOF#(Tuple2#(Bit#(13), Bit#(8))) readReqQ <- mkFIFOF;
    FIFOF#(Tuple2#(Bit#(13), Bit#(8))) readReqMetaQ <- mkFIFOF;

    FSM runTest <- mkFSM(
        (seq
            // init SrcXdma
            for (i <= 0; i < 4096; i <= i + 1) 
            seq
                action
                    fakeXdmaSrc.xdmaC2hSrv.request.put(UserLogicDmaC2hWideReq{
                        addr: zeroExtend(pack(i)),
                        len: 1,
                        dataStream: DataStreamWide{
                            data: zeroExtend(pack(i)),
                            isFirst:True,
                            isLast: True,
                            byteEn: 'h1
                        }
                    });
                endaction
                action
                    srcC2hRespQ.deq;
                endaction
            endseq
            
            // Init random
            action
                startAddrRnd.cntrl.init;
                lenRnd.cntrl.init;
            endaction

            for (i <= 0; i < 8192 * 2; i <= i + 1) 
            seq
                action
                    let a <- startAddrRnd.next;
                    let l <- lenRnd.next;
                    
                    if (a + zeroExtend(l) <= 4096 && l > 0) begin
                        readReqQ.enq(tuple2(a, l));
                    end
                endaction
            endseq
            
            delay(1000);

            // check result
            for (i <= 0; i < 4096; i <= i + 1) 
            seq
                action
                    fakeXdmaDst.xdmaH2cSrv.request.put(UserLogicDmaH2cReq{
                        addr: zeroExtend(pack(i)),
                        len: 1
                    });
                endaction
                action
                    let resp <- fakeXdmaDst.xdmaH2cSrv.response.get;
                    if ((resp.dataStream.byteEn != 1) || (resp.dataStream.data[7:0] != pack(i)[7:0]) ) begin
                        $display("Error, exxpected last byte = %x, i = %x", pack(i)[7:0], i);
                        $display("resp=", fshow(resp));
                        $finish;
                    end
                endaction
            endseq
            $display("passed");
            $finish;
        endseq)
    );

    rule sendDmaReadReq;
        let {addr, len} = readReqQ.first;
        readReqQ.deq;

        readReqMetaQ.enq(tuple2(addr, len));

        fakeXdmaSrc.xdmaH2cSrv.request.put(UserLogicDmaH2cReq{
            addr: zeroExtend(addr),
            len: zeroExtend(len)
        });
    endrule

    rule forwardDMAReadToDMAWrite;
        let {addr, len} = readReqMetaQ.first;
        UserLogicDmaH2cWideResp resp <- fakeXdmaSrc.xdmaH2cSrv.response.get;
        
        if (resp.dataStream.isLast) begin
            readReqMetaQ.deq;
        end

        // force unused bytes to 0. this makes the test more strict
        DATA_WIDE t = resp.dataStream.data;
        for (Integer i = 0; i < 64; i = i + 1) begin
            if (resp.dataStream.byteEn[i] == 0) begin
                t[i*8 + 7: i*8] = 8'h0;
            end
        end

        resp.dataStream.data = t;


        fakeXdmaDst.xdmaC2hSrv.request.put(
            UserLogicDmaC2hWideReq{
                addr: zeroExtend(addr),
                len: zeroExtend(len),
                dataStream: resp.dataStream
            }
        );
    endrule

    rule descardDmaWriteResult;
        let _ <- fakeXdmaDst.xdmaC2hSrv.response.get;
    endrule



    rule doTest if (!startedUser);
        startedUser <= True;
        runTest.start;
    endrule


endmodule