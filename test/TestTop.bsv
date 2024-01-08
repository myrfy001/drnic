import Connectable :: *;
import ClientServer :: *;
import GetPut :: *;
import Vector :: *;
import Clocks :: * ;
import StmtFSM::*;
import Randomizable :: * ;
import Axi4LiteTypes :: *;
import BRAM :: *;
import PAClib::*;
import FIFOF :: *;

import DataTypes :: *;
import UserLogicSettings :: *;
import XdmaWrapper :: *;
import RegisterBlock :: *;
// import ControlCmdManager :: *;
import UserLogicTypes :: *;
import AddressTranslate :: *;
import Ringbuf :: *;
import Arbitration :: *;
import UserLogicUtils :: *;
import CmdQueue :: *;

import TransportLayer :: *;
import WorkAndCompleteQueue :: *;

import UdpIpArpEthRxTx :: *;
import Ports :: *;
import EthernetTypes :: *;
import Utils :: *;
import SemiFifo :: *;

import Top :: *;

typedef 88 TEST_UDP_PORT;

(* doc = "testcase" *)
module mkTestTop(Empty);
    ClockDividerIfc divClk <- mkClockDivider(2);
    Clock slowClock = divClk.slowClock;
    Reset slowReset <- mkInitialReset(1, clocked_by slowClock);

    UdpIpArpEthRxTx udpA <- mkGenericUdpIpArpEthRxTx(True);
    UdpIpArpEthRxTx udpB <- mkGenericUdpIpArpEthRxTx(True);

    UdpTxPacketLenCalc lenCalcA <- mkUdpTxPacketLenCalc(1);
    UdpTxPacketLenCalc lenCalcB <- mkUdpTxPacketLenCalc(0);

    FakeXdma fakeXdmaA <- mkFakeXdma(1, tagged Hex "test_host_memory.hex", clocked_by slowClock, reset_by slowReset);
    FakeXdma fakeXdmaB <- mkFakeXdma(2, tagged Hex "test_host_memory.hex", clocked_by slowClock, reset_by slowReset);

    BsvTopCore#(CsrAddr, CsrData) bsvTopCoreA <- mkBsvTopCore(slowClock, slowReset);
    BsvTopCore#(CsrAddr, CsrData) bsvTopCoreB <- mkBsvTopCore(slowClock, slowReset);

    mkConnection(fakeXdmaA.xdmaH2cSrv, bsvTopCoreA.dmaReadClt);
    mkConnection(fakeXdmaA.xdmaC2hSrv, bsvTopCoreA.dmaWriteClt);

    mkConnection(fakeXdmaB.xdmaH2cSrv, bsvTopCoreB.dmaReadClt);
    mkConnection(fakeXdmaB.xdmaC2hSrv, bsvTopCoreB.dmaWriteClt);


    // connected by the rules below to monitor data passed between them when debugging.
    // mkConnection(toGet(bsvTopCoreA.rdmaDataStreamPipeOut), bsvTopCoreB.rdmaDataStreamInput);
    // mkConnection(bsvTopCoreA.rdmaDataStreamInput, toGet(bsvTopCoreB.rdmaDataStreamPipeOut));



    Reg#(UInt#(32)) idx <- mkReg(0);
    Reg#(Bool) startedUser <- mkReg(False);
    Reg#(UInt#(32)) stopCntReg <- mkReg(0);

    Randomize#(Bit#(13)) startAddrRnd <- mkGenericRandomizer;
    Randomize#(Bit#(8)) lenRnd <- mkGenericRandomizer;

    
    mkConnection(toGet(udpA.axiStreamTxOut), udpB.axiStreamRxIn);
    mkConnection(toGet(udpB.axiStreamTxOut), udpA.axiStreamRxIn);

    mkConnection(lenCalcA.txMetaOut, udpA.udpIpMetaDataTxIn);
    mkConnection(lenCalcB.txMetaOut, udpB.udpIpMetaDataTxIn);

    mkConnection(lenCalcA.txOut, udpA.dataStreamTxIn);
    mkConnection(lenCalcB.txOut, udpB.dataStreamTxIn);

    FSM runTest <- mkFSM(
        (seq

            udpA.udpConfig.put(UdpConfig{
                macAddr: 0,
                ipAddr: 0,
                netMask: 32'hFFFFFFFF,
                gateWay: 1
            });

            udpB.udpConfig.put(UdpConfig{
                macAddr: 1,
                ipAddr: 1,
                netMask: 32'hFFFFFFFF,
                gateWay: 0
            });
        
            // set cmd queue response ringbuf addr
            bsvTopCoreA.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: False, queueIndex: 0, regIndex: CsrIdxRbBaseAddrLow})) << 2,
                data: 'h1000
            });
            bsvTopCoreB.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: False, queueIndex: 0, regIndex: CsrIdxRbBaseAddrLow})) << 2,
                data: 'h1000
            });

            action
                let t1 <- bsvTopCoreA.csrWriteSrv.response.get;
                let t2 <- bsvTopCoreB.csrWriteSrv.response.get;
            endaction

            // set recv queue ringbuf addr
            bsvTopCoreA.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: True, queueIndex: 1, regIndex: CsrIdxRbBaseAddrLow})) << 2,
                data: 'h2000
            });
            bsvTopCoreB.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: True, queueIndex: 1, regIndex: CsrIdxRbBaseAddrLow})) << 2,
                data: 'h2000
            });

            action
                let t1 <- bsvTopCoreA.csrWriteSrv.response.get;
                let t2 <- bsvTopCoreB.csrWriteSrv.response.get;
            endaction


            // set send queue ringbuf addr
            bsvTopCoreA.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: True, queueIndex: 2, regIndex: CsrIdxRbBaseAddrLow})) << 2,
                data: 'h3000
            });
            bsvTopCoreB.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: True, queueIndex: 2, regIndex: CsrIdxRbBaseAddrLow})) << 2,
                data: 'h3000
            });

            action
                let t1 <- bsvTopCoreA.csrWriteSrv.response.get;
                let t2 <- bsvTopCoreB.csrWriteSrv.response.get;
            endaction


            // set recv complete queue response ringbuf addr
            bsvTopCoreA.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: False, queueIndex: 1, regIndex: CsrIdxRbBaseAddrLow})) << 2,
                data: 'h4000
            });
            bsvTopCoreB.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: False, queueIndex: 1, regIndex: CsrIdxRbBaseAddrLow})) << 2,
                data: 'h4000
            });

            action
                let t1 <- bsvTopCoreA.csrWriteSrv.response.get;
                let t2 <- bsvTopCoreB.csrWriteSrv.response.get;
            endaction

            // set send complete queue response ringbuf addr
            bsvTopCoreA.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: False, queueIndex: 2, regIndex: CsrIdxRbBaseAddrLow})) << 2,
                data: 'h5000
            });
            bsvTopCoreB.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: False, queueIndex: 2, regIndex: CsrIdxRbBaseAddrLow})) << 2,
                data: 'h5000
            });

            action
                let t1 <- bsvTopCoreA.csrWriteSrv.response.get;
                let t2 <- bsvTopCoreB.csrWriteSrv.response.get;
            endaction


            // move cmd queue head to init RDMA
            bsvTopCoreA.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: True, queueIndex: 0, regIndex: CsrIdxRbHead})) << 2,
                data: 13
            });
            bsvTopCoreB.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: True, queueIndex: 0, regIndex: CsrIdxRbHead})) << 2,
                data: 13
            });

            action
                let t1 <- bsvTopCoreA.csrWriteSrv.response.get;
                let t2 <- bsvTopCoreB.csrWriteSrv.response.get;
            endaction



            // read cmd resp queue head pointer to check if all cmd executed
            for (idx <= 0; idx<30; idx<=idx+1)
            seq
                bsvTopCoreA.csrReadSrv.request.put(CsrReadRequest{
                    addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: False, queueIndex: 0, regIndex: CsrIdxRbHead})) << 2
                });
                action
                    let t <- bsvTopCoreA.csrReadSrv.response.get;
                    $display("t=%d", t);
                endaction
                delay(10);
            endseq



            // move A's send queue head to emit rdma read
            bsvTopCoreA.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: True, queueIndex: 2, regIndex: CsrIdxRbHead})) << 2,
                data: 2
            });

            action
                let t <- bsvTopCoreA.csrWriteSrv.response.get;
            endaction


        endseq)
    );
    

    // mkConnection(toGet(bsvTopCoreA.rdmaDataStreamPipeOut), bsvTopCoreB.rdmaDataStreamInput);
    // mkConnection(bsvTopCoreA.rdmaDataStreamInput, toGet(bsvTopCoreB.rdmaDataStreamPipeOut));

    rule monitAndShowLineData;
        if (bsvTopCoreA.rdmaDataStreamPipeOut.notEmpty) begin
            // bsvTopCoreB.rdmaDataStreamInput.put(bsvTopCoreA.rdmaDataStreamPipeOut.first);

            bsvTopCoreA.rdmaDataStreamPipeOut.deq;
            let data = bsvTopCoreA.rdmaDataStreamPipeOut.first;
            $display("rdma_A_out = ", fshow(data));
            lenCalcA.txIn.put(data);
        end

        if (bsvTopCoreB.rdmaDataStreamPipeOut.notEmpty) begin
            // bsvTopCoreA.rdmaDataStreamInput.put(bsvTopCoreB.rdmaDataStreamPipeOut.first);

            bsvTopCoreB.rdmaDataStreamPipeOut.deq;
            let data = bsvTopCoreB.rdmaDataStreamPipeOut.first;
            $display("rdma_B_out = ", fshow(data));
            lenCalcB.txIn.put(data);
        end


        if (udpA.udpIpMetaDataRxOut.notEmpty) begin
            udpA.udpIpMetaDataRxOut.deq;
            $display("udpA recv meta = ", fshow(udpA.udpIpMetaDataRxOut.first));
        end

        if (udpA.dataStreamRxOut.notEmpty) begin
            let data = udpA.dataStreamRxOut.first;
            udpA.dataStreamRxOut.deq;

            let outData = DataStream {
                data: swapEndian(data.data),
                byteEn: swapEndianBit(data.byteEn),
                isLast: data.isLast,
                isFirst: data.isFirst
            };
            bsvTopCoreA.rdmaDataStreamInput.put(outData);
            $display("udpA recv = ", fshow(outData));
        end

        if (udpB.udpIpMetaDataRxOut.notEmpty) begin
            udpB.udpIpMetaDataRxOut.deq;
            $display("udpB recv meta = ", fshow(udpB.udpIpMetaDataRxOut.first));
        end

        if (udpB.dataStreamRxOut.notEmpty) begin
            let data = udpB.dataStreamRxOut.first;
            udpB.dataStreamRxOut.deq;
            let outData = DataStream {
                data: swapEndian(data.data),
                byteEn: swapEndianBit(data.byteEn),
                isLast: data.isLast,
                isFirst: data.isFirst
            };
            bsvTopCoreB.rdmaDataStreamInput.put(outData);
            $display("udpB recv = ", fshow(outData));
        end


    endrule

    rule doTest if (!startedUser);
        startedUser <= True;
        runTest.start;
    endrule

    rule stopTest;
        stopCntReg <= stopCntReg + 1;
        if (stopCntReg >= 10000) begin
            $finish();
        end
    endrule
endmodule


interface UdpTxPacketLenCalc;
    interface Put#(DataStream) txIn;
    interface Get#(Ports::DataStream) txOut;
    interface Get#(UdpIpMetaData) txMetaOut;
endinterface

module mkUdpTxPacketLenCalc(Integer myIp, UdpTxPacketLenCalc ifc);
    // PMTU is 4096 byte and bus width is 32 byte
    FIFOF#(Ports::DataStream) inputBuf <- mkSizedFIFOF(4096/32);
    Reg#(UdpLength) totalLenReg <- mkReg(0);

    FIFOF#(DataStream) inputDataQ <- mkFIFOF;
    FIFOF#(UdpIpMetaData) outputMetaQ <- mkFIFOF;
    
    rule recvAndAcc;
        let curTotalLen = totalLenReg;
        let inData = inputDataQ.first;
        inputDataQ.deq;
        inputBuf.enq(Ports::DataStream {
            data: swapEndian(inData.data),
            byteEn: swapEndianBit(inData.byteEn),
            isLast: inData.isLast,
            isFirst: inData.isFirst
        });

        // $display("%d recv frag =============", myIp);

        UdpLength curFragLen = 0;
        case (calcFragByteNumFromByteEn(inData.byteEn)) matches
            tagged Valid .bitNum: begin
                curFragLen = unpack(zeroExtend(pack(bitNum)));
            end
            default: begin
                $display("Get byte len from ByteEn Failed.");
                $finish;
            end
        endcase

        if (inData.isFirst) begin
            curTotalLen = curFragLen;
        end 
        else begin
            curTotalLen = curTotalLen + curFragLen;
        end

        if (inData.isLast) begin
            // $display("%d, full UDP send Packet, len = %x", myIp, curTotalLen);
            outputMetaQ.enq(UdpIpMetaData {
                dataLen: curTotalLen,
                ipAddr: fromInteger(myIp),
                ipDscp: 0,
                ipEcn:  0,
                dstPort: fromInteger(valueOf(TEST_UDP_PORT)),
                srcPort: fromInteger(valueOf(TEST_UDP_PORT))
            });
        end
             
        totalLenReg <= curTotalLen;
    endrule

    interface txIn = toPut(inputDataQ);
    interface txOut = toGet(inputBuf);
    interface txMetaOut = toGet(outputMetaQ);
endmodule