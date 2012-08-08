package pomgen

import sbt._
import Keys._

import scala.xml.{XML,Elem,NamespaceBinding,Node,Text}
import XML._
import scala.xml.transform._
import scala.xml.parsing._
import scala.xml.transform._
import java.util.Date
import java.text.SimpleDateFormat
import scala.collection.immutable.HashSet


/**
 * plugin for generating pom
 */ 
object PomGenPlugin extends sbt.Plugin{
  
  object PomGenKeys{
    val generatePomCommandName = "gen-pom"
    val pomName = SettingKey[String]("pom-name","set pom filename.default:pom.xml")
    val generatePomTaskKey = TaskKey[Unit]("generate-pom","generate and save pom")
    val composePomTaskKey = TaskKey[Elem]("compose-pom","task:compose genereated pom and existing pom")
    
  }
  
  import PomGenKeys._
  
  
  lazy val pomGenSettings = Seq(
    composePomTask,
    generatePomTask,
    pomName := "pom.xml",
    commands ++= Seq(generatePomCommand)
    /*commands ++= Seq(
      genPom
    )*/
  )
  
  
  def backupPom( baseDir : File,pomName : String) = {
    val f = new java.io.File(baseDir,pomName)
    if(f.exists){
      val pomDir = new File(baseDir,"target/pom_backup")
      if(!pomDir.exists){
        pomDir.mkdirs()
      }
      val sufix = new SimpleDateFormat("yyMMddHHmmss").format(new Date)
      val backupDest = new File(pomDir,pomName + "." + sufix)
      LogManager.defaultScreen.info("Backup old pom to %s".format(backupDest.getAbsolutePath))
      f.renameTo(backupDest)
    }
  }
  
  def detectIndent( nodes : Seq[Node]) = {
    val r = """^\s+$""".r
    nodes.find(n => n match{
      case t : Text => t.text.length > 0 && r.findFirstIn(t.text).isDefined
      case _ => false
    }) getOrElse(Text(" " * 4))
  }
  
  def mergeChilren( baseChildren : Seq[Node], replaceChild : Seq[Node]) : Seq[Node] = {
    val indent = detectIndent(baseChildren)
    var mergeElems =  replaceChild.filter( _.isInstanceOf[Elem]).groupBy(e => e.prefix + ":" + e.label)
    val replaced = baseChildren.flatMap( _ match{
      case e : Elem => {
        val key = e.prefix + ":" + e.label
        mergeElems.get(key) match{
          case Some(elems) => {
            mergeElems += (key -> Nil)
            elems.toSeq
          }
          case None => {
            Seq(e)
          }
        }
      }
      case n => n
    }) ++ mergeElems.flatMap(n => n._2.flatMap(n2 => Seq(indent,n2)))
    
    if(replaced.size > 0){
      replaced.last match{
        case Text(t) if t.contains("\n") => replaced
        case _ => replaced ++ Seq(Text("\n"))
      }
    }else{
      replaced
    }
  }
  
  
  case class ScalaVersionPropRep(scalaVersion : String) extends RewriteRule{
    override def transform(node:Node): Seq[Node] = node match{
      case Elem(_,"scala.version",_,_, _@_*) => {
        <scala.version>{scalaVersion}</scala.version>
      }
      case n => n
    }
  }
  
  def replaceScalaVersionProperty( pom : Elem , scalaVersion : String) : Elem = {
    new RuleTransformer(ScalaVersionPropRep(scalaVersion))(pom).asInstanceOf[Elem]
  }
  
  
  
  private def xml(file : File) = {
    ConstructingParser.fromSource(scala.io.Source.fromFile(file),true).document().docElem.asInstanceOf[Elem]
  }
  
  val composePomTask = composePomTaskKey <<= (scalaVersion,makePom,pomName,baseDirectory) map{ (scalaVersion,generatedPomFile,pomName,baseDir) => {
    //val generatedPom = XML.loadFile(generatedPomFile)
    val generatedPom = xml(generatedPomFile)
    def fixNamespace( n : Node, nb : NamespaceBinding) : Node = {
      n match{
        case Elem( prefix,label,metaData,_,children@_*) => {
          Elem(prefix,label,metaData,nb, children.map(fixNamespace(_,nb)) :_*)
        }
        case n => n
      }
    }
    val f = new java.io.File(baseDir,pomName)
    
    val basePom = if(f.exists()){
      xml(f)
    }else{
      LogManager.defaultScreen.info(pomName + " not found.Merge with template pom.")
      TemplateXML.template
    }
    
    val composed = basePom match{
      case e : Elem => {
        val genChildren = fixNamespace(generatedPom,e.scope).child
        e.copy(child = mergeChilren(e.child,genChildren))
      }
      case _ => {
        LogManager.defaultScreen.error("can't find base pom")
        generatedPom
      }
    }
    replaceScalaVersionProperty(composed,scalaVersion)
  }}
  
  val generatePomTask = generatePomTaskKey <<= (composePomTaskKey,pomName,baseDirectory) map{ (composedPom ,pomName, baseDir) => {
    
    backupPom(baseDir,pomName)
    
    XML.save(new File(baseDir,pomName).getAbsolutePath,composedPom)
    
    LogManager.defaultScreen.info("Success! Generate %s".format(pomName))
  
  }}
  
  val generatePomCommand : Command = Command.command(generatePomCommandName,
  "Generate and save pom","Generate pom by using make-pom command, and merge pom if already exists")( st => {
    val extracted = Project.extract(st)
    extracted.runTask(generatePomTaskKey,st)
    
    st
  })
    
  
  /*
  lazy val genPom = Command.command("gen-pom") { state =>
    println("generate pom!")
    state
  }*/
}