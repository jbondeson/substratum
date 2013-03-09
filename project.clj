(defproject antisynergy/substratum "0.0.1-SNAPSHOT"
  :description "A set of ubiquitous functions and proto-libraries."
  :url "https://github.com/jbondeson/substratum"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.5.0"]
                 ;; Logging
                 [org.clojure/tools.logging "0.2.6"] 
                 [log4j/log4j "1.2.17"
                  :exclusions [javax.mail/mail javax.jms/jms
                               com.sun.jdmk/jmxtools com.sun.jmx/jmxri]]]
  :aliases {"test!" ["do" "clean," "deps," "test"]}
  :aot :all
  :warn-on-reflection true)
