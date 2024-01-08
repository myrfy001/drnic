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




interface BsvTop#(numeric type dataSz, numeric type userSz);
    interface XdmaChannel#(dataSz, userSz) xdmaChannel;
    interface RawAxi4LiteSlave#(CSR_ADDR_WIDTH, CSR_DATA_STRB_WIDTH) axilRegBlock;
    interface Clock slowClockIfc;
    interface Put#(DataStream) rdmaDataStreamInput;
    interface DataStreamPipeOut rdmaDataStreamPipeOut;
endinterface


(* synthesize *)
module mkBsvTop(Clock slowClock, Reset slowReset, BsvTop#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH) ifc);
    XdmaWrapper#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH) xdmaWrap <- mkXdmaWrapper(clocked_by slowClock, reset_by slowReset);
    XdmaAxiLiteBridgeWrapper#(CsrAddr, CsrData) xdmaAxiLiteWrap <- mkXdmaAxiLiteBridgeWrapper(slowClock, slowReset);
    BsvTopCore#(CsrAddr, CsrData) bsvTopCore <- mkBsvTopCore(slowClock, slowReset);
    mkConnection(xdmaAxiLiteWrap.csrWriteClt, bsvTopCore.csrWriteSrv);
    mkConnection(xdmaAxiLiteWrap.csrReadClt, bsvTopCore.csrReadSrv);
    mkConnection(xdmaWrap.dmaReadSrv, bsvTopCore.dmaReadClt);
    mkConnection(xdmaWrap.dmaWriteSrv, bsvTopCore.dmaWriteClt);

    interface xdmaChannel = xdmaWrap.xdmaChannel;
    interface slowClockIfc = slowClock;
    interface axilRegBlock = xdmaAxiLiteWrap.cntrlAxil;
    interface rdmaDataStreamInput = bsvTopCore.rdmaDataStreamInput;
    interface rdmaDataStreamPipeOut = bsvTopCore.rdmaDataStreamPipeOut;
endmodule


interface BsvTopCore#(type t_csr_addr, type t_csr_data);
    interface UserLogicDmaReadWideClt dmaReadClt;
    interface UserLogicDmaWriteWideClt dmaWriteClt;
    interface Server#(CsrWriteRequest#(t_csr_addr, t_csr_data), CsrWriteResponse) csrWriteSrv;
    interface Server#(CsrReadRequest#(t_csr_addr), CsrReadResponse#(t_csr_data)) csrReadSrv;
    interface Put#(DataStream) rdmaDataStreamInput;
    interface DataStreamPipeOut rdmaDataStreamPipeOut;
endinterface


// if software and hardware try to move the same pointer, software won.
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_0.recvDmaResp" *) 
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_0.recvDmaResp_1" *) 
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_1.recvDmaResp" *) 
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_1.recvDmaResp_1" *) 
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_2.recvDmaResp" *)
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_2.recvDmaResp_1" *) 
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_3.recvDmaResp" *)
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_3.recvDmaResp_1" *) 
module mkBsvTopCore(Clock slowClock, Reset slowReset, BsvTopCore#(CsrAddr, CsrData) ifc);
    // TODO, make sure which reset to use.
    BluerdmaDmaProxy bluerdmaDmaProxy <- mkBluerdmaDmaProxy;
    RingbufPool#(RINGBUF_H2C_TOTAL_COUNT, RINGBUF_C2H_TOTAL_COUNT, RingbufRawDescriptor) ringbufPool <- mkRingbufPool;

    RegisterBlock#(CsrAddr, CsrData) regBlock <- mkRegisterBlock(ringbufPool.h2cMetas, ringbufPool.c2hMetas);
    
    
    TLB tlb <- mkTLB;
    PgtManager pgtManager <- mkPgtManager(tlb);

    function Bool isH2cDmaReqFinished(UserLogicDmaH2cReq req) = True;
    function Bool isH2cDmaRespFinished(UserLogicDmaH2cResp resp) = resp.dataStream.isLast;
    function Bool isC2hDmaReqFinished(UserLogicDmaC2hReq req) = req.dataStream.isLast;
    function Bool isC2hDmaRespFinished(UserLogicDmaC2hResp resp) = True;
    
    Vector#(4, RingbufDmaH2cClt) dmaAccessH2cCltVec = newVector;
    Vector#(2, RingbufDmaC2hClt) dmaAccessC2hCltVec = newVector;

    dmaAccessH2cCltVec[0] = bluerdmaDmaProxy.userlogicSideReadClt;
    dmaAccessH2cCltVec[1] = ringbufPool.dmaAccessH2cClt;
    dmaAccessH2cCltVec[2] = pgtManager.pgtDmaReadClt;
    dmaAccessH2cCltVec[3] <- mkFakeClient;

    dmaAccessC2hCltVec[0] = bluerdmaDmaProxy.userlogicSideWriteClt;
    dmaAccessC2hCltVec[1] = ringbufPool.dmaAccessC2hClt;

    UserLogicDmaReadClt xdmaReadClt <- mkClientArbiter(dmaAccessH2cCltVec, isH2cDmaReqFinished, isH2cDmaRespFinished);
    UserLogicDmaWriteClt xdmaWriteClt <- mkClientArbiter(dmaAccessC2hCltVec, isC2hDmaReqFinished, isC2hDmaRespFinished);

    
    XdmaGearbox xdmaGearbox <- mkXdmaGearbox(slowClock, slowReset);

    mkConnection(xdmaReadClt, xdmaGearbox.h2cStreamSrv);
    mkConnection(xdmaWriteClt, xdmaGearbox.c2hStreamSrv);


    CommandQueueController cmdQController <- mkCommandQueueController;
    mkConnection(toGet(ringbufPool.h2cRings[0]), cmdQController.ringbufSrv.request);
    // TODO: use mkCOnnection for this and the line above
    rule forwardCmdQResponseToRingbuf;
        let t <- cmdQController.ringbufSrv.response.get;
        ringbufPool.c2hRings[0].enq(t);
    endrule

    WorkRequestAndCompleteController workAndCompleteQController <- mkWorkRequestAndCompleteController;


    mkConnection(cmdQController.pgtManagerClt, pgtManager.pgtModifySrv);

    DmaReqAddrTranslator addrTranslator <- mkDmaReadReqAddrTranslator;

    let rdmaTransportLayer <- mkTransportLayer;

    // mkConnection(rdmaTransportLayer.dmaReadClt, bluerdmaDmaProxy.blueSideReadSrv);   
    // mkConnection(rdmaTransportLayer.dmaWriteClt, bluerdmaDmaProxy.blueSideWriteSrv);
    mkConnection(rdmaTransportLayer.dmaReadClt.request, tpl_2(addrTranslator.readReqTranslator));
    mkConnection(tpl_1(addrTranslator.readReqTranslator), bluerdmaDmaProxy.blueSideReadSrv.request);
    mkConnection(rdmaTransportLayer.dmaReadClt.response, bluerdmaDmaProxy.blueSideReadSrv.response);
    mkConnection(rdmaTransportLayer.dmaWriteClt.request, tpl_2(addrTranslator.writeReqTranslator));
    mkConnection(tpl_1(addrTranslator.writeReqTranslator), bluerdmaDmaProxy.blueSideWriteSrv.request);
    mkConnection(rdmaTransportLayer.dmaWriteClt.response, bluerdmaDmaProxy.blueSideWriteSrv.response);

    
    mkConnection(addrTranslator.tlbClt, tlb.find);

    mkConnection(cmdQController.metaDataManagerClt, rdmaTransportLayer.srvPortMetaData);
    

    mkConnection(workAndCompleteQController.rqRingBuf, toGet(ringbufPool.h2cRings[1]));
    mkConnection(workAndCompleteQController.sqRingBuf, toGet(ringbufPool.h2cRings[2]));
    mkConnection(workAndCompleteQController.rcqRingBuf.get, ringbufPool.c2hRings[1].enq);
    mkConnection(workAndCompleteQController.scqRingBuf.get, ringbufPool.c2hRings[2].enq);
    mkConnection(workAndCompleteQController.recvReq, rdmaTransportLayer.recvReqInput);
    mkConnection(workAndCompleteQController.workReq, rdmaTransportLayer.workReqInput);
    mkConnection(workAndCompleteQController.workCompRQ, toGet(rdmaTransportLayer.workCompPipeOutRQ));
    // mkConnection(workAndCompleteQController.workCompSQ, toGet(rdmaTransportLayer.workCompPipeOutSQ));

    rule debugWorkCompSQ;
        let t = rdmaTransportLayer.workCompPipeOutSQ.first;
        rdmaTransportLayer.workCompPipeOutSQ.deq;
        workAndCompleteQController.workCompSQ.put(t);
        // $display("WorkCompSQ Recv = ", fshow(t));
    endrule


    interface csrWriteSrv = regBlock.csrWriteSrv;
    interface csrReadSrv = regBlock.csrReadSrv;
    interface dmaReadClt = xdmaGearbox.h2cStreamClt;
    interface dmaWriteClt = xdmaGearbox.c2hStreamClt;
    interface rdmaDataStreamInput = rdmaTransportLayer.rdmaDataStreamInput;
    interface rdmaDataStreamPipeOut = rdmaTransportLayer.rdmaDataStreamPipeOut;
endmodule

function Bit#(width) swapEndian(Bit#(width) data) provisos(Mul#(8, byteNum, width));
    Vector#(byteNum, Bit#(BYTE_WIDTH)) dataVec = unpack(data);
    return pack(reverse(dataVec));
endfunction

function Bit#(width) swapEndianBit(Bit#(width) data) provisos(Mul#(1, byteNum, width));
    Vector#(byteNum, Bit#(1)) dataVec = unpack(data);
    return pack(reverse(dataVec));
endfunction


