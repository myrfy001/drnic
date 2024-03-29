import DataTypes :: *;

typedef 0 USER_LOGIC_XDMA_TUSER_WIDTH;
typedef 512 USER_LOGIC_XDMA_DATA_WIDTH;
typedef TDiv#(USER_LOGIC_XDMA_DATA_WIDTH, 8) USER_LOGIC_XDMA_KEEP_WIDTH;

// Make sure USER_LOGIC_DESCRIPTOR_BIT_WIDTH * USER_LOGIC_RING_BUF_DEEP = 4kB
typedef 32   USER_LOGIC_DESCRIPTOR_BYTE_WIDTH;
typedef TMul#(USER_LOGIC_DESCRIPTOR_BYTE_WIDTH, BYTE_WIDTH)  USER_LOGIC_DESCRIPTOR_BIT_WIDTH; // 256 bit
typedef 128  USER_LOGIC_RING_BUF_DEEP; 
typedef TLog#(USER_LOGIC_RING_BUF_DEEP)  USER_LOGIC_RING_BUF_DEEP_WIDTH ; 



typedef 4 RINGBUF_H2C_TOTAL_COUNT;
typedef 4 RINGBUF_C2H_TOTAL_COUNT;
typedef TAdd#(RINGBUF_H2C_TOTAL_COUNT, RINGBUF_C2H_TOTAL_COUNT) RINGBUF_TOTAL_COUNT;
typedef TLog#(RINGBUF_TOTAL_COUNT) RINGBUF_NUMBER_WIDTH;


typedef 16 CMD_QUEUE_DESCRIPTOR_MAX_SEGMENT_CNT;