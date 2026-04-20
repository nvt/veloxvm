(import "radio")

(println "Channel: " (get-channel))
(println "RSSI: " (get-rssi))
(set-txpower 14)
(println "TX power: " (get-txpower))
