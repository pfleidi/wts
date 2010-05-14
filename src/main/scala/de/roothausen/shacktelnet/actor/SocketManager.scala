package de.roothausen.shacktelnet.actor

import _root_.net.liftweb._
import actor._
import http._
import _root_.scala.actors.Actor
import _root_.scala.actors.Actor._
import _root_.java.io._
import _root_.java.net.{InetAddress,ServerSocket,Socket,SocketException}


case class Command(cmd: String)
case class Response(resp: String)
case class Disconnect


class SocketManager(hostName: String, callBack: CometActor) extends Actor {

  private val ia = InetAddress.getByName(hostName)
  private val socket = new Socket(ia, 23)
  private val out = new DataOutputStream(new DataOutputStream(socket.getOutputStream()))
  private val in = new DataInputStream(socket.getInputStream())
  private val receiverThread = new Receiver(in, callBack)


  def act() {
    receiverThread.start()
    loop {
      react {
        case Disconnect => disconnect
        case Command(cmd) => sendCommand(cmd)
        case _ => println("Bogus cmd!")
      }
    }
  }

  private def disconnect: Unit = {
    receiverThread.stop()
    out.close()
    in.close()
    socket.close()
    exit()
  }

  private def sendCommand(cmd: String): Unit = {
    println("sendCmd:" + cmd)
    out.writeBytes(cmd + "\n")
    out.flush()
  }

}


class Receiver(val in: DataInputStream, val actor: CometActor) extends Thread {

  private val inReader = new BufferedReader(new InputStreamReader(in))

  override def run() {
    while(true) {
      while(inReader.ready()) {
        val line = inReader.readLine()
        println("receive:" + line)
        actor ! Response(line)
      }
      Thread.sleep(500)
    }
  }

}
