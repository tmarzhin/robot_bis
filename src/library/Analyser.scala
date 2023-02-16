package library

/**
 *  Scrap the webpage, gather the urls , filter and return the good ones
 *  @return List of Title,URL
 */

object Analyser extends AnalysePage {
	
	def resultats(url: String, exp: Expression): List[(String, String)] =	{
			val hurl: Html = try {
				UrlProcessor.fetch(url);
			} catch {
				case e: Exception => throw e
			}
			println("Analyse de : " + url)

			val listAnnonce: List[Html] = try {
				getListAnnonce(hurl)
			} catch {
				case e: Exception => throw e
			}

			val res: List[(String, String)] = filtrerAnnonces(listAnnonce, exp)
			println("Result founded : " + res.size)
			res
		}

	/**
	 * Parcours la liste des annonces et renvoie la liste des (titre, lien) qui
	 * correspondent à l'expression
	 * 
	 * @param listAnnonce, la liste des annonces
	 * @param exp, l'expression
	 */
	private def filtrerAnnonces(listAnnonce: List[Html], exp: Expression): List[(String, String)] = {
		listAnnonce match {
			case Nil => Nil
			case html :: end => if (AnalyserHtml.filtreHtml(html, exp)) (getTitle(html), getLink(html)) :: filtrerAnnonces(end, exp)
			else filtrerAnnonces(end, exp)
		}
	}
	
	/**
	 * Retourne la liste des annonces sur la page html fournis.
	 * Cherche la liste des announces tel que présente dans les pages
	 * de résultat de vivastreet
	 * 
	 * @param html, le html d'une page resultat vivastreet
	 */
	private def getListAnnonce(html: Html): List[Html] = {
		html match {
			case Text(text) => Nil
			case Tag(name, attr, child) if (name == "ul" && getAttributValue(attr, "class") == "list") => child
			case Tag(name, attr, child) => getListAnnonceInChild(child)
		}
	}
	
	/**
	 * Permet de chercher recursivement dans les enfants d'un Tag
	 * avec une fonction en parametre. Retourne la liste des resultat
	 *
	 * @param l, la liste des enfants
	 * @param func, la fonction qui cherche
	 */
	def getListAnnonceInChild[A](l: List[Html]): List[Html] = {
		l match {
			case Nil => Nil
			case h :: end => getListAnnonce(h) ++ getListAnnonceInChild(end)
		}
	}
	
	 /**
	  * Retourne le titre de l'annonce vivastreet fournie en parametre
	  * Cherche la div ou est présente le titre et appel getName pour
	  * trouver le texte.
	  * 
	  * @param html, le html d'une annonce vivastreet
	  */
	private def getTitle(html: Html): String = {
		html match {
			case Text(text) => throw NotFound
			case Tag(name, attr, child) if (name == "div" && getAttributValue(attr, "class") == "clad__title") => AnalyserHtml.html2String(child)
			case Tag(name, attr, child) => getTitleInChild(child)
		}
	}
	
	def getTitleInChild(l: List[Html]): String = {
		l match {
			case Nil => throw NotFound
			case h :: end => 
				try {
				  getTitle(h)
				} catch {
					case e: Exception => getTitleInChild(end) 
				}
		}
	}
	
	/**
	 * Retourne le texte de la balise h4 présent dans le html
	 * 
	 * @param html, le html dans lequel chercher la balise h4
	 */
	private def getName(html: Html): String = {
		html match {
			case Text(txt) => throw NotFound
			case Tag(name, attr, Text(text) :: Nil) if (name == "h4") => text
			case Tag(name, attr, child) => getNameInChild(child)
		}
	}

	def getNameInChild(l: List[Html]): String = {
		l match {
			case Nil => throw NotFound
			case h :: end => 
				try {
				  getName(h)
				} catch {
					case e: Exception => getNameInChild(end) 
				}
		}
	}
	
	/**
	 * Retourne le lien de l'annonce vivastreet fournie en parametre
	 * 
	 * @param html, le html d'une annonce vivastreet
	 */
	private def getLink(html : Html): String = {
		val links: List[String] = AnalyserURL.filtreAnnonce(html)
		if (links.isEmpty) "error"
		else links(0)
	}

	/**
	 * Renvoie la valeur associée à l'attribut en parametre
	 *
	 * @param attrs, la liste des attribut d'un Tag
	 * @param cle, la cle de l'attribut
	 */
	def getAttributValue(attrs: List[(String, String)], cle: String): String = {
		attrs match {
			case Nil => ""
			case (n, v) :: end => if (n == cle) v else getAttributValue(end, cle)
		}
	}

	case object NotFound extends Exception
}










