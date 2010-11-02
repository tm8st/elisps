;;; init-skk.el --- skk init

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: skk, init
;; creation time: Sun Jul  4 16:13:43 2010
;;; Commentary:

;;; Code:

(require 'skk)

(global-set-key (kbd "C-c C-SPC") 'skk-mode)
(setq skk-tut-file "~/elisps/external/ddskk-20100704/etc/SKK.tut")

(customize-set-value 'skk-kakutei-key "\C-o")
;; (customize-set-value 'skk-kakutei-key "\C-o")
;; (customize-set-value 'skk-kakutei-key "\C-m")
(customize-set-value 'skk-show-inline t)
;; (customize-set-value 'skk-show-inline 'vertical)
(customize-set-value 'skk-auto-insert-paren t)

(when my-initialized
  (add-hook 'find-file-hook
	    '(lambda ()
	       (skk-mode t)
	       (skk-latin-mode-on)
	       )))

;; C-\ �ł� SKK �ɐ؂�ւ�����悤�ɐݒ�
(setq default-input-method "japanese-skk")
;; ���艼���������ɐ���������D�悵�ĕ\��
(setq skk-henkan-strict-okuri-precedence t)
;;�����o�^���A���艼���������ɐ����������`�F�b�N
(setq skk-check-okurigana-on-touroku t)

;;;-------------------------------
;;; �\���̐ݒ�
;;;-------------------------------
;; ���b�Z�[�W����{��Œʒm����
(setq skk-japanese-message-and-error t)
;; ���j���[���p��ŕ\������
(setq skk-show-japanese-menu t)

;; �ϊ����ɒ��� (annotation) ��\������
(setq skk-show-annotation nil)

;;isearch-mode �ɓ������ۂɎ����I�� skk-isearch ���N��
;; (add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
;; (add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)

;; �ϊ����ꗗ�ƒ��� (annotation) �� GUI �ۂ��\������
(setq skk-show-tooltip nil)

;;tooltip�̐F�ݒ�
;; (when skk-show-tooltip
;;   (setq skk-tooltip-parameters
;;         '((background-color . "black")
;; 	  (foreground-color . "white")
;;           (border-color . "royal blue"))))

;; (customize-set-value 'skk-show-tooltip nil)
;; (customize-set-value 'skk-tooltip-y-offset -30)
;; (customize-set-value 'skk-tooltip-parameters
;; 		     '((foreground-color . "navy blue")
;; 		       (background-color . "alice blue")
;; 		       (border-color . "royal blue")
;; 		       (border-width . 2)))
;; (set-face-foreground 'skk-dcomp-multiple-face "white")

(customize-set-value 'skk-use-color-cursor t)
;; SKK ���[�h���I�t�ł��邱�Ƃ������J�[�\���F�B�W���ł́A�J�[�\���̂���Y�� �t���[���ɂ�����W���̃J�[�\���F���g���܂��B
;; (customize-set-value 'skk-cursor-default-color t)

;; ���ȃ��[�h�ł��邱�Ƃ������J�[�\���F�B�W���ł́A�w�i�̖��Âɂ�� "coral4" �܂��� "pink" ��p���܂��B
(customize-set-value 'skk-cursor-hiragana-color "pink")
 
;; �J�i���[�h�ł��邱�Ƃ������J�[�\���F�B�W���ł́A�w�i�̖��Âɂ�� "forestgreen" �܂��� "green" ��p���܂��B
(customize-set-value 'skk-cursor-katakana-color "red")
;; skk-cursor-katakana-color

;; �A�X�L�[���[�h�ł��邱�Ƃ������J�[�\���F�B�W���ł́A�w�i�̖��Âɂ�� "ivory4" �܂��� "gray" ��p���܂��B
(customize-set-value 'skk-cursor-latin-color "ivory")

;;�R�����g�s�𔲂�����ascii�ɂ���B
(add-hook 'skk-load-hook
          (lambda ()
            (require 'context-skk)))

;; �ϊ������C�����C���ɕ\������
(setq skk-show-inline t)

;; �ϊ������c�^�C�����C���ɕ\������
;; (setq skk-show-inline 'vertical)

(when skk-show-inline
  ;; �ϐ� skk-treat-candidate-appearance-function �𗘗p���Ď��O�Ō���
  ;; �F��t����ꍇ�͂��̕ϐ��� nil �ɐݒ肷��B
  (setq skk-inline-show-face nil))

;;;-------------------------------
;;; ��{�I�ȃ��[�U�E�C���^�[�t�F�[�X
;;;-------------------------------

;; Enter �L�[���������Ƃ��ɂ͊m�肷��
;; (setq skk-egg-like-newline t)
(setq skk-egg-like-newline t)

;; �Ή�������ʂ������I�ɑ}������
(setq skk-auto-insert-paren t)

;; ��Ǔ_�𓮓I�Ɍ��肷��
(add-hook 'skk-mode-hook
          (lambda ()
            (save-excursion
              (goto-char 0)
              (make-local-variable 'skk-kutouten-type)
              (if (re-search-forward "�B" 10000 t)
                  (setq skk-kutouten-type 'en)
                (setq skk-kutouten-type 'jp)))))

;; ���I�ȕ⊮���g��
(setq skk-dcomp-activate t)

;; ���I�⊮�̉ۂ𔻒肷���荂�x�Ȑݒ��
(setq skk-dcomp-activate
      #'(lambda ()
          (and
           ;; -nw �ł͓��I�⊮�����Ȃ��B
           window-system
           ;; ��{�I�ɍs���̂Ƃ��̂ݕ⊮����B�������s���łȂ��Ă����݂�
           ;; �|�C���g����s���܂ł̕������󔒂݂̂�������⊮����B
           (or (eolp)
               (looking-at "[ \t]+$")))))

;; ���I�⊮�Ō��𕡐��\������
(setq skk-dcomp-multiple-activate t)

;;;-------------------------------
;;; �ϊ�����̒���
;;;-------------------------------
;; ���艼���������ɐ���������D�悵�ĕ\������
(setq skk-henkan-strict-okuri-precedence t)
;; �����o�^�̂Ƃ��A�]�v�ȑ��艼���𑗂�Ȃ��悤�ɂ���
(setq skk-check-okurigana-on-touroku 'auto)
;; �ϊ��̊w�K
(require 'skk-study)
;;�P���������̃L�[��!�ɂ���
(setq skk-tankan-search-key ?!)

;;;-------------------------------
;;; �����Ɋ֘A�����ݒ�
;;;-------------------------------
;; look �R�}���h���g��������������
(setq skk-use-look t)

(when skk-use-look
  ;; look ��������������o����Ƃ��Č�������
  (setq skk-look-recursive-search t)
  ;; ispell �� look �ƈꏏ�Ɏg���̂͂�߂�
  (setq skk-look-use-ispell nil)
  ;; look �ɓn���R�}���h���C���I�v�V�����̐ݒ�B�⊮���ƌ��������ꂼ���
  ;; ���Đݒ�ł���B
  ;; look �� case ������Ƃ��́A�����p�̎����� sort �R�}���h�ō��K�v
  ;; ������ (look �̈��� -d, -f �� sort �̈��� -d, -f �ƈ�v�����Ă����K
  ;; �v������)�B
  ;; (*) �⊮���ɂ͈��� -d ���w�肷��� dcomp �Ƃ̕��p���ɖ�肠�邱�Ƃ�
  ;; �񍐂���Ă��邽�߁A-d ���w�肵�Ȃ����Ƃ������߂��܂��B
  (setq skk-look-completion-arguments "%s /usr/share/dict/words")
  (setq skk-look-conversion-arguments "-df %s /usr/share/dict/words")
  ;; `skk-abbrev-mode' �� skk-look ���g���������������Ƃ��Ɋm�����
  ;; �l�����ɋL�^���Ȃ��悤�ɂ���
  (add-hook 'skk-search-excluding-word-pattern-function
            ;; KAKUTEI-WORD �������ɂ��ăR�[�������̂ŁA�s�v�ł����������
            ;; �K�v����
            #'(lambda (kakutei-word)
                (and skk-abbrev-mode
                     (save-match-data
                       ;; `skk-henkan-key' �� "*" �ŏI���Ƃ��A�܂���
                       ;; `skk-henkan-key' �������݂̂̂Ƃ�
                       (or (string-match "\\*$" skk-henkan-key)
                           (string-match "^[0-9]*$" skk-henkan-key)))))))

;; ���l�ϊ��@�\���g��
(setq skk-use-numeric-conversion t)

;; �J�^�J�i���ϊ����ɉ�����B
(setq skk-search-prog-list
      (skk-nunion skk-search-prog-list
                  '((skk-search-katakana))))
;;;-------------------------------
;;; �����Ɋւ���ݒ�
;;;-------------------------------
;; �����T�[�o���g�����߂̐ݒ�
(setq skk-server-host "localhost")
(setq skk-server-portnum 1178)

;; ������ Emacsen ���N�����Čl���������L����
(setq skk-share-private-jisyo t)

;; 10 �����u����ƌl�����������I�ɕۑ������ݒ�
(defvar skk-auto-save-jisyo-interval 600)
(defun skk-auto-save-jisyo ()
  (skk-save-jisyo)
  )
(run-with-idle-timer skk-auto-save-jisyo-interval
                     skk-auto-save-jisyo-interval
                     'skk-auto-save-jisyo)

;;;-------------------------------
;;; ���̑����낢��
;;;-------------------------------
;; ���ȃ��[�h�̓��͂� (���[�h�ύX���s�Ȃ킸��) ����(�[)��
;; ASCII �����̒���ł� `-' �ɁA�S�p�����̒���ł� `?' �ɂ������B
(setq skk-rom-kana-rule-list
	  (cons '("-" nil skk-hyphen)
			skk-rom-kana-rule-list))

(defun skk-hyphen (arg)
  (let ((c (char-before (point))))
    (cond ((null c) "�[")
          ((and (<= ?0 c) (>= ?9 c)) "-")
          ((and (<= ?�O c) (>= ?�X c)) "?")
          (t "�["))))

;; ���ȃ��[�h�̓��͂Ń��[�h�ύX���s�킸�ɁA�������͒���
;; �����_ (.) ����уJ���} (,) ���͂���������B
;; (��) ���ȃ��[�h�̂܂� 1.23 �� 1,234,567 �Ȃǂ̋L�q���s����B
;; period
(setq skk-rom-kana-rule-list
	  (cons '("." nil skk-period)
			skk-rom-kana-rule-list))
(defun skk-period (arg)
  (let ((c (char-before (point))))
    (cond ((null c) "�B")
          ((and (<= ?0 c) (>= ?9 c)) ".")
          ((and (<= ?�O c) (>= ?�X c)) "�D")
          (t "�B"))))

;; comma
(setq skk-rom-kana-rule-list
	  (cons '("," nil skk-comma)
			skk-rom-kana-rule-list))
(defun skk-comma (arg)
  (let ((c (char-before (point))))
    (cond ((null c) "�A")
          ((and (<= ?0 c) (>= ?9 c)) ",")
          ((and (<= ?�O c) (>= ?�X c)) "�C")
          (t "�A"))))

(provide 'init-skk)
