package library
import java.io.FileWriter
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.awt.Desktop;

object MonApp extends App {
 
  // Debut et fin du lien, inserer les mots cle au debut
  
  val milieuLien = "?lb=new&search=1&start_field=1&keywords="
  val finLien = "&cat_1=&geosearch_text=&searchGeoId=0"
  
  // Demande à l'utilisateur de rentrer des mot cle et genere une expression
  val exp: Expression = ExpressionParser.readExp
  
  // Creation d'une liste de lien correspondant à la recherche
  val urls: List[String] = getLinksFromKeywords(getKeywords(exp), 10);
  
  // Creation d'une liste de lien correspondant à la requête
  val analyseResultat: List[(String,String)] = analysePages(urls, exp).toList
  
  // Creation d'une du fichier html correspondant à la requête
  val html: Html = ProductionResultatD.resultat2html(analyseResultat, exp)
  val htmlString = HtmlToString.process(html)
  
  //print(htmlString)
  
  
  // Creer un fichier et ecrit le code html
  val file: File = new File("monFichier.html")
  val fileStream: FileOutputStream = new FileOutputStream(file) // Ne correspond pas au code dans le sujet pour forcer l'encodage en UTF-8 et eviter d'eventuelle soucis
  val writer: OutputStreamWriter = new OutputStreamWriter(fileStream, "UTF-8")
  try{
    writer.write(htmlString)
  } finally writer.close()
  
  // Ouvre la page html
  val desktop: Desktop = Desktop.getDesktop();
  if(file.exists()) desktop.open(file);
  
   /**
   * A partir d'une liste d'URL de requête sur le site de reference et d'une expression exp,
   * retourne les pages issues de la requête et satisfaisant l'expression.
   * 
   * Renvoie un Set pour garantir qu'un resultat present dans deux requêtes n'apparaise pas en double
   * 
   * @param urls, la liste des urls a analyser
   * @param exp, l'expresion correspondant a la recherche
   */
  private def analysePages(urls: List[String], exp: Expression): Set[(String,String)] = {
    urls match {
      case Nil => Set()
      case url :: end => 
        try {
          Analyser.resultats(url, exp).toSet ++ analysePages(end, exp)
        } catch {
          case e:Exception => e.printStackTrace(); analysePages(end, exp)
        }
    }
  }
  
   /**
   * Creer une liste avec tous les mot-cle d'une Expression
   * Les mots assoscies avec And sont concatene sous la forme : And("mot1","mot2") = "mot1+mot2"
   * 
   * @param exp, l'Expression 
   */
   def getKeywords(exp: Expression): List[String] = {
    exp match {
      case Word(w) => List(w)
      case Or(exp1 , And(exp2, exp3)) => getKeywords(exp1) ++ getKeywords(exp2) ++ getKeywords(exp3)
      case And(exp1, exp2: And) => concatKeywords1(getKeywords(exp1), getKeywords(exp2))
      case And(exp1, exp2) => concatKeywords2(getKeywords(exp1),getKeywords(exp2))
      case Or(exp1, exp2) => getKeywords(exp1) ++ getKeywords(exp2)
    }
  }
   
   /**
    * Premiere methode de concatenation des mots cles pour l'association avec un And dans un Ou
    * 
    * @param l1, la premiere liste des mot-cle a concatene
    * @param l2, la seconde liste des mot-cle a concatene
    */
   private def concatKeywords1(l1: List[String], l2: List[String]): List[String] = {
    (l1, l2) match {
      case (exp1 :: Nil, exp2 :: end2) => List(exp1 + "+" + exp2) ++ end2
      case _ => l1 ++ l2
    }
  }
  
   /**
    * Seconde methode de concatenation des mots cles pour un And avec deux element
    * 
    * @param l1, la premiere liste des mot-cle a concatene
    * @param l2, la seconde liste des mot-cle a concatene 
    */
  private def concatKeywords2(l1: List[String], l2: List[String]): List[String] = {
    (l1, l2) match {
      case (exp1 :: Nil, exp2 :: Nil) => List(exp1 + "+" + exp2)
      case _ => l1 ++ l2
    }
  
  }
  
  /**
   * Prend une liste de mot-cle et genere la liste des lien de recherche pour ces mot-cle
   * sur vivastreet. Pour chaque mot-cle genere le lien des nb premiere page de resultat.
   * 
   * @param keywords, la liste des mot-cle
   * @param nb, le nombre de page de resultat souhaite
   */
  def getLinksFromKeywords(keywords: List[String], nb: Int): List[String] = {
    keywords match {
      case Nil => Nil
      case keyword :: end => getLinksFromKeyword(milieuLien + keyword + finLien, List(), nb) ++ getLinksFromKeywords(end, nb)
    }
  }
  
  /**
   * Prend un mot-cle et genere les liens des nb premiere page de resultat pour ce mot-cle
   * 
   * @param fin, la fin du lien
   * @param l, une liste de depart,
   * @param nb, le nombre de page de resultat souhaite
   */
  def getLinksFromKeyword(fin: String, l: List[String], nb: Int): List[String] = {
    val debutLien = "https://search.vivastreet.com/annonces/fr"
    if (nb == 1) (debutLien + fin) :: l
    else getLinksFromKeyword(fin, l, nb-1) :+ (debutLien + "/t+" + nb + fin)
  }

}