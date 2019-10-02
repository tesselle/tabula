#' Boves ceramics
#'
#' A dataset containing the ceramic counts from the castle site of Boves
#'  (Somme, France). The data are grouped into eight periods ranging from the
#'  10th to the 18th century and thirteen ceramic types.
#' @format A numeric matrix with 8 rows and 13 variables (ceramic types):
#'  \describe{
#'    \item{I}{}
#'    \item{IIa}{}
#'    \item{IIb}{}
#'    \item{IIIa}{}
#'    \item{IIIb}{}
#'    \item{IIIc}{}
#'    \item{IVa}{}
#'    \item{IVb}{}
#'    \item{Va}{}
#'    \item{Vb}{}
#'    \item{VI}{}
#'    \item{VII}{}
#'  }
#' @source
#'  Racinet P. (2002). Le site castral et prioral de Boves du Xe au XVIIe
#'  siècle. Bilan des recherches 1996-2000. \emph{Revue archéologique de
#'  Picardie}. Numéro spécial 20, 123 p.
#' @family datasets
#' @keywords datasets
"boves"

#' Compiègne ceramics
#'
#' A dataset containing the ceramic counts from the Place des Hallettes in
#'  Compiègne (Oise, France). The data are grouped into five periods of about a
#'  century, ranging from the 9th to the 14th century, and sixteen ceramic
#'  types.
#' @format A numeric matrix with 5 rows (chronological periods, numbered from
#'  the oldest to the most recent from 1 to 5) and 16 variables (ceramic types):
#'  \describe{
#'   \item{A}{Red to white ceramics with fine sized inclusions.}
#'   \item{B}{Red to white ceramics with medium sized inclusions.}
#'   \item{C}{Dark ceramics with fine sized inclusions.}
#'   \item{D}{Dark ceramics with medium sized inclusions.}
#'   \item{E}{Ceramics close to those of groups B or D, with similarities to
#'   group F.}
#'   \item{F}{Black, red or beige ceramics with coarse inclusions.}
#'   \item{G}{Red polished ceramics with fine to medium sized inclusions.}
#'   \item{H}{Black polished ceramics with fine sized inclusions.}
#'   \item{I}{Black polished ceramics with medium sized inclusions.}
#'   \item{J}{Polished and painted ceramics with fine to medium sized
#'   inclusions.}
#'   \item{K}{Painted ceramics, similar to those of group A.}
#'   \item{L}{Painted ceramics, similar to those of group B.}
#'   \item{M}{Painted ceramics with coarse inclusions.}
#'   \item{N}{Glazed ceramics.}
#'   \item{O}{Stamped ceramics.}
#'   \item{P}{Coated ceramics.}
#'  }
#' @source
#'  Lacroix, M. C. (1997). La céramique médiévale du site des Hallettes à
#'  Compiègne (Oise). \emph{Revue archéologique de Picardie}. Numéro spécial,
#'  13(1), 135-168.
#'  DOI: \href{https://doi.org/10.3406/pica.1997.1945}{10.3406/pica.1997.1945}
#' @family datasets
#' @keywords datasets
"compiegne"

#' Merzbach ceramics
#'
#' A dataset containing the ceramic counts from the Merzbach assemblage
#' (Germany). The data are grouped into eight phases.
#' @format A numeric matrix with 8 rows (phases, numbered from VII to XIV)
#' and 36 variables (pottery motifs).
#'  \describe{
#'   \item{BT1, BT2, ...}{ Counts of a motif.}
#'  }
#' @source
#'  Crema, E. R., Kandler, A. & Shennan, S. (2016). Revealing Patterns of
#'  Cultural Transmission from Frequency Data: Equilibrium and Non-Equilibrium
#'  Assumptions. \emph{Scientific Reports}, 6(1).
#'  DOI: \href{https://doi.org/10.1038/srep39122}{10.1038/srep39122}.
#'
#'  Crema, E. R. (2016). Sample codes and data for "Revealing patterns of
#'  cultural transmission from frequency data: equilibrium and non-equilibrium
#'  assumptions". \emph{Zenodo}, v1.0.
#'  DOI: \href{https://doi.org/10.5281/zenodo.187558}{10.5281/zenodo.187558}.
#' @family datasets
#' @keywords datasets
"merzbach"

#' Mississippi ceramics
#'
#' A dataset containing ceramic counts from the Mississippi region.
#' @format A numeric matrix with 20 rows and 10 variables (ceramic types):
#'  \describe{
#'    \item{ParkinPunctate}{}
#'    \item{BartonKentMPI}{}
#'    \item{Painted}{}
#'    \item{FortuneNoded}{}
#'    \item{RanchIncised}{}
#'    \item{WallsEngraved}{}
#'    \item{WallaceIncised}{}
#'    \item{RhodesIncised}{}
#'    \item{VernonPaulApplique}{}
#'    \item{HullEngraved}{}
#'  }
#' @source
#'  Lipo, C. P., Madsen, M. E. & Dunnell, R. C. (2015). A
#'  Theoretically-Sufficient and Computationally-Practical Technique for
#'  Deterministic Frequency Seriation. \emph{PLOS ONE}, 10(4), e0124942.
#'  DOI: \href{https://doi.org/10.1371/journal.pone.0124942}{10.1371/journal.pone.0124942}.
#' @family datasets
#' @keywords datasets
"mississippi"

#' Zuni ceramics
#'
#' A dataset containing ceramic counts from the Zuni region of the American
#'  Southwest.
#' @format A numeric matrix with 420 rows (assemblages) and 18 variables
#'  (ceramic types). The numbers in brackets correspond to the date range of
#'  each type (in AD years):
#'  \describe{
#'    \item{LINO}{Lino Gray (575-875).}
#'    \item{KIAT}{Kiatuthlanna Black-on-white (850-910).}
#'    \item{RED}{Red Mesa Black-on-white (900-1030).}
#'    \item{GALL}{Gallup Black-on-white (1025-1150).}
#'    \item{ESC}{Escavada Black-on-white (1050-1150).}
#'    \item{PUBW}{Puerco Black-on-white (1050-1200).}
#'    \item{RES}{Reserve Black-on-white (1071-1115).}
#'    \item{TULA}{Tularosa Black-on-white (1175-1300).}
#'    \item{PINE}{Pinedale Black-on-white (1275-1325).}
#'    \item{PUBR}{Puerco Black-on-red (1050-1200).}
#'    \item{WING}{Wingate Black-on-red (1070-1200).}
#'    \item{WIPO}{Wingate Polychrome (1150-1250).}
#'    \item{SJ}{St. Johns Black-on-red/Polychrome (1200-1300).}
#'    \item{LSJ}{St. Johns glaze, Techado Polychrome (1275-1300).}
#'    \item{SPR}{Springerville Polychrome (1250-1300).}
#'    \item{PINER}{Pinedale Black-on-red/Polychrome (1275-1325).}
#'    \item{HESH}{Heshotauthla Polychrome (1285-1400).}
#'    \item{KWAK}{Kwakina Polychrome (1285-1400).}
#'  }
#' @source
#'  Peeples, M. A., & Schachner, G. (2012). Refining correspondence
#'  analysis-based ceramic seriation of regional data sets. \emph{Journal of
#'  Archaeological Science}, 39(8), 2818-2827.
#'  DOI: \href{https://doi.org/10.1016/j.jas.2012.04.040}{10.1016/j.jas.2012.04.040}.
#' @family datasets
#' @keywords datasets
"zuni"
