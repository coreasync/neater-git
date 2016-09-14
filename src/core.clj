(ns core
  (:require [clojure.java.io :as io]
            [clj-jgit.porcelain :as p])
  (:import [org.eclipse.jgit.api Git]))

(defn mcontains? [coll key] (not (nil? (some #{key} coll))))

;;TODO fix handling . - getParent returns null
;;TODO "." and ".." return `true`
(defn sym-link?-old[f]
  (let [canon (if (.getParent f)
                (io/file (-> f .getParentFile .getCanonicalFile) (.getName f))
                f)]
    (not= (.getCanonicalFile canon) (.getAbsoluteFile canon))))

(defn sym-link?[f]
  (java.nio.file.Files/isSymbolicLink (.toPath (io/file f))))


(defn get-git-dirs[d]
  (let[all (->> d io/file .listFiles seq)
       dirs (filter #(.isDirectory %) all)
       is-git-dir (mcontains? (map #(.getName %) dirs) ".git")]
    (if is-git-dir
      [(.getCanonicalPath d)]
      (reduce concat (map get-git-dirs dirs)))))

(defn git-repo-clean? [d]
  (p/with-repo d
    (->>
     (p/git-status repo)
     vals
     (apply clojure.set/union)
     empty?)))

;;check if there are local files (.gitignore)
(defn git-repo-untracked [d]
  (p/with-repo d
    (->>
     (p/git-status repo)
     :untracked
     (remove #(-> % clojure.java.io/file sym-link?))
     not-empty)))

(defn ll[x] (prn x) x)
(defn git-repo-touched [d]
  (p/with-repo d
    (->>
     (->
      (p/git-status repo)
      (dissoc :untracked)
      vals)
     (apply clojure.set/union)
     not-empty)))

(defn git-repo-local-commits [d] ;;this is the first version which might not be accurate
  (p/with-repo d
    (let [commit-ref-f (fn [e] [(.getName (.getObjectId e)) (.getName e)])
          commit-ref-groups-f (fn [scope] (group-by first (map commit-ref-f (p/git-branch-list repo scope))))
          keys-f (fn[ps] (-> ps keys set))
          diff-f (fn [d1 d2] (mapcat d1 (clojure.set/difference (keys-f d1) (keys-f d2))))
          local (commit-ref-groups-f :local)
          remote (commit-ref-groups-f :remote)]
      {:extra (diff-f local remote) :missing (diff-f remote local)})))


(defn git-ls-remote-repository [d] (->> (-> (Git/lsRemoteRepository) (.setRemote d) (.setHeads true) (.call)) (map #(.getName %))))

(defn git-ls-remote [d]
  (p/with-repo d
    (p/git-ls-remote repo)))

(defn git-list-stash [d]
  (p/with-repo d
    (p/git-list-stash repo)))

(defn git-branch-list [d]
  (p/with-repo d
    (p/git-branch-list repo :remote)))

(defn git-get-remote[d]
  (let [config (p/with-repo d (->> repo .getRepository .getConfig))
        remotes (.getSubsections config "remote")]
    (map #(.getString config "remote" % "url") remotes)))

(defn ls-unclean-git-dirs[d]
  (let[fd (io/file d)
       all (->> fd .listFiles seq (remove #(#{".neater" ".DS_Store"} (.getName %))) (remove sym-link?))
       [dirs files] ((juxt filter remove) #(.isDirectory %) all)
       is-git-dir (mcontains? (map #(.getName %) dirs) ".git")
       pd (.getPath fd)]
    (when is-git-dir
      #_(when-not (git-repo-clean? d)
          (println pd "is not clean "))
      (when-let [ut (git-repo-untracked d)]
        (println pd "contains untracked " ut))
      (when-let [ut (git-repo-touched d)]
        (println pd "contains touched " ut))
      (when (empty? (git-get-remote d))
        (println pd "has no remotes"))
      (when-let [lc (-> d git-repo-local-commits :extra not-empty)]
        (println pd "branches missmatch:" lc))
      (when-let [ls (-> d git-list-stash vec not-empty)]
        (println pd "stash is present" ls))
      )
    ;;use  for better output
    (when-not is-git-dir
      (if (not-empty files)
        (println pd "non Git with local files" #_files)
        (when (empty? dirs)
            (println pd "non empty"))))
    (when-not is-git-dir
          (reduce concat (map ls-unclean-git-dirs dirs)))))

;;sync local files using soft links to gitlocal
;;create a file with state to clone everything from GitHub/recover from a file with a state
;;fetch and check for forced pushes
;;consolidate git functions into a my lib
;; git, gitlocal, gitdb, gitwip, nongit, nongitcopy
;; account for nested git repos
;; add stash detection
