import bonus_event_sourcing._
import cons_et_nil._
import pas_suivant._
import premiers_pas._
import support.{TestFailed, HandsOnSuite}
import type_classes._
import un_sac_avec_des_items._
import we_need_to_go_deeper._


trait HandsOn {
  def nestedSuites: List[HandsOnSuite]

}

object HandsOnScala extends App {
  def nestedSuites = List(
    new premiers_pas,
    new pas_suivant,
    new we_need_to_go_deeper,
    new cons_et_nil,
    new type_classes,
    new un_sac_avec_des_items,
    new bonus_event_sourcing
  )

  import util.control.Breaks._

  breakable {
    for (suite <- nestedSuites;
         subsuite <- suite.nestedSuites;
         (testName, testFun) <- subsuite.testRegister
    ) {

      try {
        testFun()

      } catch {
        case e: Exception => {
          subsuite.reportToTheStopper.apply(TestFailed(Option(e), "", testName))
          break()
        }
      }


    }


  }
}

class premiers_pas extends HandsOn {
  override def nestedSuites = List(
    new e0_vars_vals,
    new e1_classes,
    new e2_case_classes,
    new e3_boucle_for
  )
}

class pas_suivant extends HandsOn {
  override def nestedSuites = List(
    new e4_listes,
    new e5_maps,
    new e6_sets,
    new e7_option,
    new e8_fonctions_de_plus_haut_niveau,
    new e9_extracteurs_et_patterns
  )
}

class we_need_to_go_deeper extends HandsOn {
  override def nestedSuites = List(
    new e0_une_histoire_de_sacs,
    new e1_un_sac_comme_generique,
    new e2_un_sac_algebrique,
    new e3_un_sac_covariant

  )
}

class cons_et_nil extends HandsOn {
  override def nestedSuites = List(
    new e0_list,
    new e1_bonus_stream
  )
}

class type_classes extends HandsOn {
  override def nestedSuites = List(
    new testJson,
    new client.testJsonClient
  )
}

class un_sac_avec_des_items extends HandsOn {
  override def nestedSuites = List(
    new e0_une_mise_en_abime,
    new e1_un_peu_plus_generique,
    new e2_un_peu_plus_algebrique,
    new e3_on_a_besoin_de_la_covariance
  )
}

class bonus_event_sourcing extends HandsOn {
  override def nestedSuites = List(
    new testEventSourcing
  )
}
