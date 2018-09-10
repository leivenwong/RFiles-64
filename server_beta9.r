#install.packages("int64")
#R lacks a default 64-bit integer class which means data is stored either as a 32-bit integer or as a double
# options(digits=22)

#read and write socket use tlv format
#tag 4byte,v 4byte,反正没几个字节。。。
#TAG_TLVLIST <- 0 # L表示后面的tlv的个数
#一次传输的内容是一个tlv的list，最开始有4字节的byte计算的长度，然后有4字节的个数计算的长度
#r可以忽略byte计算的长度或用来做校验，c用这个长度做接收数据的标准，收完再一次性处理
TAG_INT <- 1
TAG_INT64 <- 2 #8BYTES
TAG_DOUBLE <- 3 #8BYTES
TAG_STRING <- 4 #NO NULL AT END

#返回list的vector，稍后实现
ReadSTLVLIST <- function(con) {
  
}

ReadByte <- function(con) {
  return (readBin(con, what="integer", 1, size=1))
}

ReadShort <- function(con) {
  return (readBin(con, what="integer", 1, size=2))
}

ReadInt <- function(con) {
  return (readBin(con, what="integer", 1))
}

ReadInt64 <- function(con) {
  #R lacks a default 64-bit integer class which means data is stored either as a 32-bit integer or as a double
  return (readBin(con, what="numeric", 1, size=8))
  #why ???，读c的就要用下面的？
  #out <- readBin(con, what="integer", 1, size=8)
  #print(mode(out))
  #print(out)
  #print(as.integer(out))
  #return (out)
}

ReadDouble <- function(con) {
  return (readBin(con, what="double", 1))
}

ReadString <- function(con, len) {
  return (readChar(con, len))
}



WriteByte <- function(con, v) {
  writeBin(as.integer(v), con, size=1)
}

WriteShort <- function(con, v) {
  writeBin(as.integer(v), con, size=2)
}

WriteInt <- function(con, v) {
  #print("WriteInt")
  #print(mode(v))
  #print(v)
  writeBin(as.integer(v), con)
}

WriteInt64 <- function(con, v) {
  #print("WriteInt64")
  #print(mode(v))
  #print(v)
  #writeBin(as.integer(v), con, size=8)
  writeBin(v, con, size=8)
}

WriteDouble <- function(con, v) {
  #print("WriteDouble")
  #print(mode(v))
  #print(v)
  writeBin(as.double(v), con)
}

WriteString <- function(con, v) {
  #print("WriteString")
  #print(mode(v))
  #print(v)
  writeChar(v, con, nchar(v))
}

#==================================================


CreateTLVIntObject <-function(V) {
  return (list(t=TAG_INT, l=4, v=V))
}

CreateTLVInt64Object <-function(V) {
  return (list(t=TAG_INT64, l=8, v=V))
}

CreateTLVDoubleObject <-function(V) {
  return (list(t=TAG_DOUBLE, l=8, v=V))
}

CreateTLVStringObject <-function(V) {
  out <- (list(t=TAG_STRING, l=nchar(V), v=V))
  #print("in CreateTLVStringObject")
  #print(out)
  return (out)
}


ReadTLVIntObject <-function(con) {
  size <- ReadInt(con) 
  value <- ReadInt(con) 
  return (list(t=TAG_INT, l=size, v=value))
}

ReadTLVInt64Object <-function(con) {
  size <- ReadInt(con) 
  value <- ReadInt64(con) 
  return (list(t=TAG_INT64, l=size, v=value))
}

ReadTLVDoubleObject <-function(con) {
  size <- ReadInt(con) 
  value <- ReadDouble(con)
  return (list(t=TAG_DOUBLE, l=size, v=value))
}

ReadTLVStringObject <-function(con) {
  size <- ReadInt(con) 
  value <- ReadString(con, size+1)
  return (list(t=TAG_STRING, l=size, v=value))
}

ReadTLVObject <- function(con, tag) {
  if (tag == TAG_INT) {
    #print("read int tlv object")
    return (ReadTLVIntObject(con))
  } else if (tag == TAG_INT64) {
    #print("read int64 tlv object")
    return (ReadTLVInt64Object(con))
  } else if (tag == TAG_DOUBLE) {
    #print("read double tlv object")
    return (ReadTLVDoubleObject(con)) 
  } else if (tag == TAG_STRING) {
    #print("read string tlv object")
    return (ReadTLVStringObject(con))
  } else {
    #print("error TLV data!!!!!!!!!!")
    #print(tag)
    return (0)
  }
}

WriteTLVObject <- function(con, tlv) {
  if (tlv$t == TAG_INT) {
    WriteInt(con, TAG_INT)
    WriteInt(con, 4)
    WriteInt(con, tlv$v)
    return (12) #返回写的字节数目
  } else if (tlv$t == TAG_INT64) {
    WriteInt(con, TAG_INT64)
    WriteInt(con, 8)
    WriteInt64(con, tlv$v)
    return (16) 
  } else if (tlv$t == TAG_DOUBLE) {
    WriteInt(con, TAG_DOUBLE)
    WriteInt(con, 8)
    WriteDouble(con, tlv$v)
    return (16) 
  } else if (tlv$t == TAG_STRING) {
    WriteInt(con, TAG_STRING)
    WriteInt(con, tlv$l)
    WriteString(con, tlv$v)
    return (8 + tlv$l)
  } else {
    #print("error TLV data!!!!!!!!!!")
    return (0)
  }
}

CalculateTLVObjectSize <- function(tlv) {
  #print("CalculateTLVObjectSize begin")
  if (tlv$t == TAG_INT) {
    return (12)
  } else if (tlv$t == TAG_INT64) {
    return (16) 
  } else if (tlv$t == TAG_DOUBLE) {
    return (16) 
  } else if (tlv$t == TAG_STRING) {
    #writeChar会追加一个结束符，但是我的长度计算的是不包括结束符的。但传输数据的总长度需要计算
    return (8 + 1 + tlv$l)
  } else {
    #print("error TLV data!!!!!!!!!!")
    return (0)
  }
}

CalculateTLVListtSize <- function(tlv_list) {
  #print("CalculateTLVListtSize begin")
  total <- 4
  #print(total)
  
  for (tlv in tlv_list) {
    total <- total + CalculateTLVObjectSize(tlv)
  }
  
  #print(total)
  #print("CalculateTLVListtSize end")
  return (total)
}

WriteTLVLIST <- function(con, v) {
  #print("WriteTLVLIST begin")
  total <- CalculateTLVListtSize(v);
  WriteInt(con, total)
  WriteInt(con, length(v))
  for (tlv in v) {
    WriteTLVObject(con, tlv)
  }
  #print("WriteTLVLIST end")
}
#=====================================================
PrintTLVObject <- function(tlv) {
  str(tlv)
  #print("tag ")
  #print(tlv$t)
  #print("len")
  #print(tlv$l)
  #print("value")
  #print(tlv$v)
}




#for test
direction <- 1
counter <- 0
WangFuncitonDemo <- function(type, level, time, time_string, open, high, low, close) {
  
  
  # 1 type string 品种
  # 2 level string 级别，MQ1S，MQ5S，MQ5M，MQ15M
  # 3 double time utc时间的微秒数目
  # 4 string time_string utc时间的字符串，秒格式
  # 5 double open开盘价
  # 6 double high 最高价
  # 7 double low 最低价
  # 8 double close 收盘价
  
  
  #1 name string 策略的名字 “ms.successive.one”
  #2 os_name string 下单策略的名字 “os.fixprice” "os.pursueprice" 分别为定价，追价
  #3 type string 品种
  #4 level int 周期
  #5 price double 表示执行价格
  #6 limit_percent double 表示超过此百分比，交易取消
  #7 position_hand double 表示一手对应的资金量，总资金量/这个数值 = 报单的手数
  #8 action int 1表示有效ms，0表示无效ms
  #9 buy_clean_sell int 1表示必须持多仓，0 表示必须空仓 -1 表示必须持空仓
  
  
  out <- list(length=9)
  
  
  
  name <- "ms.successive.one"
  os_name <- "os.fixprice"
  #os_name <- "os.pursueprice"
  
  out[[1]] <- CreateTLVStringObject(name)
  out[[2]] <- CreateTLVStringObject(os_name)
  out[[3]] <- CreateTLVStringObject(type)
  out[[4]] <- CreateTLVStringObject(level)
  out[[5]] <- CreateTLVDoubleObject(close)
  out[[6]] <- CreateTLVDoubleObject(0.01)
  out[[7]] <- CreateTLVDoubleObject(300000)
  out[[8]] <- CreateTLVIntObject(1)
  out[[9]] <- CreateTLVIntObject(0)
  
  
  
  
  
  
  #只有5m行情，IF1312才行动，别的都不行动
  if (type == "IF1401" && level == "MQ1M") {
    out[[8]] <- CreateTLVIntObject(1)
    out[[9]] <- CreateTLVIntObject(direction)
    if (direction == 1) {
      direction <<- -1
    } else {
      direction <<- 1
    }
    
  } else {
    out[[8]] <- CreateTLVIntObject(0)
  }
  
  
  
  return (out)
}

SendDataMSAction <- function(con, out) {
  real_out <- list(length=10)
  real_out[[1]] <- CreateTLVStringObject("MS_DEMO")
  
  index <- 2
  for(i in out) {
    real_out[[index]] <- i
    index <- index + 1
  }
  
  WriteTLVLIST(con, real_out)
}

CreateMSAction <- function(con, type, level, time, time_string, open, high, low, close) {
  #print("CreateMSAction begin")
  
  #print(type)
  #print(level)
  #print(time)
  #print(time_string)
  #print(open)
  #print(high)
  #print(low)
  #print(close)
  
  #call wang's function 
  #第一种策略响应
  out <- WangFunciton(type, level, time, time_string, open, high, low, close);
  if (length(out) == 9) {
    SendDataMSAction(con, out)
  }
  
  #第二种策略响应
  out2 <- WangFunciton2(type, level, time, time_string, open, high, low, close);
  if (length(out2) == 9) {
    SendDataMSAction(con, out2)
  }
  
  if (level == "MQ5S") {
      #在5s行情中判断是否要调整止损
      
      out <- WangAdjustStop(type, level, time, time_string, open, high, low, close);
      if (length(out) == 5) {
        #发送
        WriteTLVLIST(con, out)
      }
      #out <- list(length=5)
      
      #out[[1]] <- CreateTLVStringObject("R2CTP.Position.StopPrice")
      #out[[2]] <- CreateTLVStringObject("ms.successive.if.one")
      #out[[3]] <- CreateTLVStringObject(type)
      #out[[4]] <- CreateTLVIntObject(1)
      #out[[5]] <- CreateTLVDoubleObject(close + 1)
      
      
  }
}


CreateMQLevelTLVList <- function() {
  # 1 string MQ
  # 2 string type
  # 3 string level
  # 4 int64 time
  # 5 string time_string
  # 6 double open
  # 7 double high
  # 8 double low
  # 9 double close
  
  
  out <- list(length=9)
  #out[1] <- CreateTLVStringObject("MQ")
  out[[1]] <- list(t=TAG_STRING, l=2, v="MQ")
  #str(list(t=TAG_STRING, l=2, v="MQ"))
  #print("after out[1] <- CreateTLVStringObject")
  #PrintTLVObject(out[[1]])
  #print("after out[1] <- CreateTLVStringObject ..........")
  
  
  out[[2]] <- CreateTLVStringObject("IF1401")
  out[[3]] <- CreateTLVStringObject("MQ5m")
  out[[4]] <- CreateTLVInt64Object(13020311706000000)
  out[[5]] <- CreateTLVStringObject("2013-08-07 09:15:06")
  out[[6]] <- CreateTLVDoubleObject(2268.1)
  #if (FALSE) {
  out[[7]] <- CreateTLVDoubleObject(2268.2)
  out[[8]] <- CreateTLVDoubleObject(2268.3)
  out[[9]] <- CreateTLVDoubleObject(2268.4)
  #}
  #out <- c(CreateTLVStringObject("MQ"),
  #  CreateTLVStringObject("IF1401"),
  #  CreateTLVStringObject("MQ5m"),
  #  CreateTLVInt64Object(13020311706000000),
  #  CreateTLVStringObject("2013-08-07 09:15:06"),
  #  CreateTLVDoubleObject(2268.1),
  #  CreateTLVDoubleObject(2268.2),
  #  CreateTLVDoubleObject(2268.3),
  #  CreateTLVDoubleObject(2268.4))
  
  #只有5m行情，IF1312才行动，别的都不行动
  if (type != "IF1401" || level != "MQ5M")
    
    #print("CreateMQLevelTLVList over")
    #print(mode(out))
    #print(length(out))
    #print(mode(out[1]))
    #print(length(out[1]))
    #str(out[1])
    return (out)
}


SendMQLevelTLVList <- function(con) {
  v <- CreateMQLevelTLVList()
  #print(length(v))
  for (i in v) {
    #print(mode(v[i]))
    #PrintTLVObject(i)
  }
  #print("before WriteTLVLIST")
  WriteTLVLIST(con, v)
}

ReceiveData <- function(con) {
  #print("ReceiveData begin")
  total <- ReadInt(con)
  #print("after total receivedata")
  #print(Sys.time())
  #print("total")
  #print(total)
  
  number <- ReadInt(con)
  #print("number")
  #print(number)
  out <- list(length=number)
  for (i in 1:number) {
    tag <- ReadInt(con)
    #print("tag")
    #print(tag)
    out[[i]] <- ReadTLVObject(con, tag)
    #print("after ReadTLVObject")
    #PrintTLVObject(out[[i]])
  }
  #print("ReceiveData end")
  
  return (out)
}
#=========================================
HandleMQ <- function(con, tlv_list) {
  #out <- 
  CreateMSAction(con, tlv_list[[2]]$v, tlv_list[[3]]$v, tlv_list[[4]]$v, tlv_list[[5]]$v, tlv_list[[6]]$v, tlv_list[[7]]$v, tlv_list[[8]]$v, tlv_list[[9]]$v)
  #print(out)
  #WriteTLVLIST(con, out)
}


HandlePositionCost <- function(tlv_list) {
  PrintTLVObject(tlv_list)
  WangPositionCost(tlv_list[[1]]$v, tlv_list[[2]]$v, tlv_list[[3]]$v, tlv_list[[4]]$v, tlv_list[[5]]$v)
}

#这个版本是r，py统一框架版本的客户端模式，r主动连接服务器，并且在最开始发送一个握手字符串
Run <-function() {
  IFstop.one <- 0
  IFstopwin.one <- 0
  IFstop.two <- 0
  IFstopwin.two <- 0
  #IFzhunbei()
  #RUzhunbei()
  #RBzhunbei()
  #while(TRUE) {
  con1 <- socketConnection(port = 6789, server = FALSE, blocking = TRUE, open="r+b")#多进程6789.单6790
  #print("send shakehand")
  shakehand <- "tlv://rlang/ms"
  len <- nchar(shakehand)
  WriteInt(con1, len + 1)
  #WriteString(con1, shakehand)
  writeChar(shakehand, con1, nchar(shakehand))
  #print("prepare receive")
  
  while(TRUE) {
    #print("before receivedata")
    #print(Sys.time())
    tlv_list <- ReceiveData(con1)
    #print("after receivedata")
    #print(Sys.time())
    
    #print(tlv_list)
    #print(length(tlv_list))
      if (length(tlv_list) > 1) {
          cmd <- tlv_list[[1]]$v

          if (cmd == "MQ") {
              if (length(tlv_list) == 9) {
                HandleMQ(con1, tlv_list)
              } else {
                print("erro mq!!!!")
              }
            } else if (cmd == "QUITR") {
              print("ctp ask to quit")
              break
            } else if (cmd == "Order.Stop") {
              if (length(tlv_list) == 4) {
                  WangFunctionStop(tlv_list[[2]]$v, tlv_list[[3]]$v)
              }
            } else if (cmd == "Order.StopWin") {
              if (length(tlv_list) == 4) {
                  WangFunctionStopWin(tlv_list[[2]]$v, tlv_list[[3]]$v)
              }
            } else if (cmd == "CTP2R.Position.Cost") {
              if (length(tlv_list) == 6) {
                  HandlePositionCost(tlv_list)
                  #WangFunctionStopWin(tlv_list[[2]]$v, tlv_list[[3]]$v)
              }
            }

        } else {
          print("not enough or too much param")
        }
  }
  close(con1)
  #}
}


#=======================================================
Run()
