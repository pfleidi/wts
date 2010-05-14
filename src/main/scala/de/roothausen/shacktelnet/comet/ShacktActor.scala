package de.roothausen.shacktelnet.comet

import _root_.net.liftweb._
import http._
import common._
import actor._
import util._
import Helpers._
import _root_.scala.xml._
import S._
import SHtml._
import js._
import JsCmds._
import JE._
import net.liftweb.http.js.jquery.JqJsCmds.{AppendHtml}

import _root_.de.roothausen.shacktelnet.actor._

class ShackActor extends CometActor {

  private val hostName = "localhost"
  private val socketManager = new SocketManager(hostName, this)

  private lazy val inputArea = findKids(defaultXml, "tlnt", "input")
  private lazy val bodyArea = findKids(defaultXml, "tlnt", "body")
  
  def render = bind("tlnt", bodyArea, "resp" -> respSpan)
  def respSpan = <div id="resp" />

  override lazy val fixedRender: Box[NodeSeq] =
    ajaxForm(bind("tlnt", inputArea,
                  "input" -> text("", sendCommand _)))

  override def localSetup = {
    socketManager.start()
  }

  override def localShutdown = {
    socketManager ! Disconnect
  }

  override def lowPriority = {
    case Response(response) => {
        println("Got response!");
        partialUpdate(AppendHtml("resp", <p>{Text(response)}</p>))
      }
  }

  private def sendCommand(cmd: String) {
    socketManager ! Command(cmd)
  }

}
