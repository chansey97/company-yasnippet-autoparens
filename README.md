# company-yasnippet-autoparens
A company-mode script for auto-parenthesis

### Usage

0. Install company 和 yasnippet

2. Copy company-yasnippet-autoparens.el to any path which emacs can load (eg: In windows, C:\Users\\%YOU-USERNAME%\AppData\Roaming\\.emacs.d\)

3. Add following code to your configuration
```
 (defvar company-mode/enable-yas t
   "Enable yasnippet for all backends.")

 ;; Add yasnippet support for all company backends
 (defun company-mode/backend-with-yas (backend)
   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
       backend
     (append (if (consp backend) backend (list backend))
             '(:with company-yasnippet))))

 (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

 ;; Add company-yasnippet-autoparens
 (defun company-mode/backend-with-yas-ap (backend)
   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet-autoparens backend)))
       backend
     (append (if (consp backend) backend (list backend))
             '(:with company-yasnippet-autoparens))))

 (setq company-backends (mapcar #'company-mode/backend-with-yas-ap company-backends))
```
