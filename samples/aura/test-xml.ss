(import (aura))

(define order-xml
  (sgml
   `(purchase-order
     (order ((id 100))))))      

((order-xml 'text))
