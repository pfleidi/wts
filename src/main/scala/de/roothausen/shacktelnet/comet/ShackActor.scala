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

  private val hostName = "23.shack"
  private val socketManager = new SocketManager(hostName, this)

  private lazy val inputArea = findKids(defaultXml, "wts", "input")
  private lazy val bodyArea = findKids(defaultXml, "wts", "body")
  
  def render = bind("wts", bodyArea, "resp" -> respSpan)
  def respSpan = <div id="resp" style="width: 100%; height: 500px; overflow: auto;"/>

  override lazy val fixedRender: Box[NodeSeq] =
    ajaxForm(bind("wts", inputArea,
                  "input" -> text("", sendCommand _)))

  override def localSetup = {
    socketManager.start()
  }

  override def localShutdown = {
    socketManager ! Disconnect
  }

  override def lowPriority = {
    case Response(response) => {
        partialUpdate(AppendHtml("resp", <p>{VT100.parse(response)}</p>))
      }
  }

  private def sendCommand(cmd: String) {
    socketManager ! Command(cmd)
  }

}


object VT100 {
  import _root_.scala.util.matching.Regex
  import _root_.scala.xml._


  def parse(tocheck: String): NodeSeq =  {
    val colorToken = """\e\[(([\d]{0,2})?(;[\d]{0,2})*)m([^\e\[]*)""".r

    try {
      colorToken.findAllIn(tocheck).map (_ match {
          case colorToken(m1, m2, m3, m4) =>
            <span style={getAttribute(m1)}>{m4}</span>
          case rest: String => <span>{rest}</span>
        }).reduceLeft[NodeSeq] { (acc, n) => acc ++ n }
    } catch {
      case ex: UnsupportedOperationException =>  <span />
    }

  }

  private def getAttribute(code: String): String = {
    try {
      if (code.contains(";")) {
        code.split(";").reduceLeft[String] { (rest, color) => rest + " " + getColor(color.toInt) }
      } else {
        getColor(code.toInt)
      }
    } catch {
      case ex: NumberFormatException => ""
    }
  }

  private def getColor(colorCode: Int): String = {
    colorCode match {
      case 30 => "color: black;"
      case 31 => "color: red;"
      case 32 => "color: green;"
      case 33 => "color: yellow;"
      case 34 => "color: blue;"
      case 35 => "color: magenta;"
      case 36 => "color: cyan;"
      case 37 => "color: white;"

      case 40 => "background: black;"
      case 41 => "background: red;"
      case 42 => "background: green;"
      case 43 => "background: yellow;"
      case 44 => "background: blue;"
      case 45 => "background: magenta;"
      case 46 => "background: cyan;"
      case 47 => "background: white;"

      case _ => ""
    }
  }
}
