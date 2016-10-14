;;; Copyright (C) 2011-2013 Alessio Stalla
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(asdf:defsystem :bacteria
  :name "bacteria"
  :description "Bacteria is a lightweight dataflow library, inspired by Cells but simpler and independent from CLOS. Bacteria is compilable to JavaScript using Parenscript."
  :author "Alessio Stalla <alessiostalla@gmail.com>"
  :licence "GPLv3"
  :serial t
  :depends-on (:alexandria)
  :components
  ((:static-file "bacteria.asd")
   (:file "packages")
   (:file "bacteria")))
