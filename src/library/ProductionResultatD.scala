package library

import java.io.FileReader
import java.nio.file._

object ProductionResultatD extends ProductionResultat {
  
  /** A partir d'une liste de couples (titre,URL), produit un document Html, qui
   *  liste les solutions sous la forme de liens cliquables
   *  
   *  @param l la liste des couples solution (titre,URL)
   *  @return le document Html listant les solutions
   */
  def resultat2html(l:List[(String,String)], exp: Expression):Html = {
    Tag("html",List(),
        List(
          Tag("head",List(),
            List(
              Tag("meta",List(("content","text/html"),("charset","UTF-8")),List()),
              Tag("title",List(),List(Text("Resultat de la Requete"))))
          ),
          getStyle(),
          generateBody(l, Tag("body",List(),List(Tag("div", List(("class","aff")), List(
            Tag("label",List(("class","rechercheT")),List(Text("Mot-clé : "))),
            Tag("label",List(("class","recherche")),List(Text(exp2String(exp))))    
          )))))
        )
    )
  }
  
  private def generateBody(l:List[(String,String)], html: Html): Html = {
    if (l.isEmpty) {
      Tag("div", List(("class","block")), 
        List(
            Tag("p", List(("class","notFound")), List(Text("No result found")))
        )
      )
    } else {
      generateBodyRec(l, html)
    }
  }

  
  /**
   * Créer le body de la page html
   * 
   * @param l la liste des couples solution (titre,URL)
   * @param html la balise html courante
   */
  private def generateBodyRec(l:List[(String,String)], html: Html): Html = {
      (l, html) match {
        case (Nil, h) => h
        case (elemt, Text(txt)) => Text(txt)
        case (((titre, lien) :: end), Tag(name,attr,ch)) => generateBodyRec(end, Tag(name,attr,ch :+ generateDiv(titre, lien)))
      }
  }
  
  /**
   * Créer un balise div à partir d'un titre et d'un lien
   * 
   * @param titre, le titre
   * @param lien, le lien
   */
  private def generateDiv(titre: String, lien: String): Html = {
    Tag("div", List(("class","block")), 
        List(
            Tag("a", List(("href",lien), ("class","titre"),("target","_blank")), List(Text(titre))),
            Tag("a", List(("href",lien), ("class","lien"),("target","_blank")), List(Text(lien)))
        )
    )
  }
  
  /**
   * Recupère le style du fichier
   */  
  private def getStyle(): Html = {
    try {
      val style: String = new String(Files.readAllBytes(Paths.get("style.css")))
      Tag("style", List(), List(Text(style)))
    } catch {
      case e: NoSuchFileException => Text("")
    }
  }
  
  private def exp2String(exp: Expression): String = {
    exp match {
      case Word(w) => w
      case And(e1, e2) => exp2String(e1) + " <em>and</em> " + exp2String(e2)
      case Or(e1, e2) => exp2String(e1) + " <em>or</em> " + exp2String(e2)
    }
  }
}