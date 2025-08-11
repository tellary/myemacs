(require 'lsp-ui)
(require 'lsp-mode)

(defun my-lsp-java--locate-server-command ()
  (let ((server-cmd "jdtls"))
    `(,server-cmd
      "--jvm-arg=-Dlog.protocol=true"
      "--jvm-arg=-Dlog.level=ALL"
      ,@(mapcar (lambda (str) (concat "--jvm-arg=" str)) lsp-java-vmargs))
    )
  )

;; Copy of `lsp-register-client` from `lsp-java` that uses `my-lsp-java--locate-server-command`.
;; Never tries to install anything -- just uses pre-installed jdtls.
(lsp-register-client
 (make-lsp--client
  :new-connection (lsp-stdio-connection #'my-lsp-java--locate-server-command)
  :major-modes '(java-mode java-ts-mode jdee-mode)
  :server-id 'my-jdtls
  :multi-root t
  :notification-handlers (ht ("language/status" #'lsp-java--language-status-callback)
                             ("language/actionableNotification" #'lsp-java--actionable-notification-callback)
                             ("language/progressReport" #'lsp-java--progress-report)
                             ("workspace/notify" #'lsp-java--workspace-notify)
                             ("language/eventNotification" #'ignore))
  :request-handlers (ht ("workspace/executeClientCommand" 'lsp-java-boot--workspace-execute-client-command))
  :action-handlers (ht ("java.apply.workspaceEdit" #'lsp-java--apply-workspace-edit)
                       ("java.action.generateToStringPrompt" #'lsp-java--action-generate-to-string)
                       ("java.action.hashCodeEqualsPrompt" #'lsp-java--action-generate-equals-and-hash-code)
                       ("java.action.organizeImports" #'lsp-java--action-organize-imports)
                       ("java.action.overrideMethodsPrompt" #'lsp-java--override-methods-prompt)
                       ("java.action.generateAccessorsPrompt" #'lsp-java--generate-accessors-prompt)
                       ("java.action.generateConstructorsPrompt" #'lsp-java--generate-constructors-prompt)
                       ("java.action.applyRefactoringCommand" #'lsp-java--apply-refactoring-command)
                       ("java.action.rename" #'lsp-java--action-rename)
                       ("java.show.references" #'lsp-java--show-references)
                       ("java.show.implementations" #'lsp-java--show-implementations))
  :uri-handlers (ht ("jdt" #'lsp-java--resolve-uri))
  :initialization-options (lambda ()
                            (list :settings (lsp-configuration-section "java")
                                  :extendedClientCapabilities
                                  (list :progressReportProvider (lsp-json-bool lsp-java-progress-reports-enabled)
                                        :classFileContentsSupport t
                                        :classFileContentsSupport t
                                        :overrideMethodsPromptSupport t
                                        :hashCodeEqualsPromptSupport t
                                        :advancedOrganizeImportsSupport t
                                        :generateConstructorsPromptSupport t
                                        :generateToStringPromptSupport t
                                        :advancedGenerateAccessorsSupport t
                                        :advancedExtractRefactoringSupport t
                                        :moveRefactoringSupport t
                                        :resolveAdditionalTextEditsSupport t)
                                  :bundles (lsp-java--bundles)
                                  :workspaceFolders (->> (lsp-session)
                                                         lsp-session-server-id->folders
                                                         (gethash 'jdtls)
                                                         (-uniq)
                                                         (-map #'lsp--path-to-uri)
                                                         (apply #'vector))))
  :library-folders-fn (lambda (_workspace) (list lsp-java-workspace-cache-dir))
  :before-file-open-fn (lambda (_workspace)
                         (let ((metadata-file-name (lsp-java--get-metadata-location buffer-file-name)))
                           (setq-local lsp-buffer-uri
                                       (when (file-exists-p metadata-file-name)
                                         (with-temp-buffer (insert-file-contents metadata-file-name)
                                                           (buffer-string))))))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration (lsp-configuration-section "java"))
                      (lsp--server-register-capability
                       (lsp-make-registration
                        :id "test-id"
                        :method "workspace/didChangeWatchedFiles"
                        :register-options? (lsp-make-did-change-watched-files-registration-options
                                            :watchers
                                            (vector (lsp-make-file-system-watcher :glob-pattern "**/*.java")
                                                    (lsp-make-file-system-watcher :glob-pattern "**/pom.xml")
                                                    (lsp-make-file-system-watcher :glob-pattern "**/*.gradle")
                                                    (lsp-make-file-system-watcher :glob-pattern "**/.project")
                                                    (lsp-make-file-system-watcher :glob-pattern "**/.classpath")
                                                    (lsp-make-file-system-watcher :glob-pattern "**/settings/*.prefs")))))))
  :completion-in-comments? t

  :download-server-fn #'lsp-java--ensure-server))
