(defpackage :reddit-best
  (:use :common-lisp :drakma :cl-json))
(in-package :reddit-best)

;; drakma json encoding
(push (cons "application" "json") *text-content-types*)

;; HTTP Basic Authorization is used
(defun http-auth-basic (url username password &rest data)
  (decode-json-from-string (http-request url
		      :method :post
		      :basic-authorization `(,username ,password)
		      :parameters data)))

(defparameter +auth_url+ "https://www.reddit.com/api/v1/access_token") 
(defun authorize (client-id secret-id username password)
  "Returns an oauth code"
  (cdar (http-auth-basic +auth_url+
			 client-id
			 secret-id
			 '("grant_type" . "password")
			 `("username" . ,username)
			 `("password" . ,password))))

(defparameter +base_url+ "https://oauth.reddit.com/")

(defmacro build-url (&rest parts)
  `(concatenate 'string ,@parts))

(defun authorized-req (url code &rest data)
  (decode-json-from-string (http-request (build-url +base_url+ url)
		 :additional-headers `(("Authorization" .
							,(concatenate 'string "bearer " code)))
		 :parameters data)))

(defun api-me (code)
  (authorized-req "api/v1/me" code))

(defun api-subreddit-hot (code sub)
  (cdr (nth 3 (assoc :data (authorized-req (concatenate 'string "r/" sub "/hot") code)))))

(defun permalink (link)
  (concatenate 'string "https://reddit.com" link))

(defun format-post (post)
  (let ((title (cdr (assoc :title post)))
	(link (permalink (cdr (assoc :permalink post)))))
    (format t "TITLE:~t~a~%" title)
    (format t "LINK:~t~a~%" link)
    (format t "~%")))

(defun format-posts (posts)
  (loop for post in posts
     do (format-post (cdr (assoc :data post)))))

(defun format-subreddit-top (code subreddit)
  (format-posts (api-subreddit-hot code subreddit)))
