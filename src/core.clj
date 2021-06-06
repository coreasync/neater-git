(ns core
  (:require[clojure.java.io :as io]
           [clj-jgit.porcelain :as p]
           [clojure.string :as str])
  (:import[org.eclipse.jgit.api Git]))

(defn mcontains?[coll key] (not (nil? (some #{key} coll))))

;;TODO fix handling . - getParent returns null
;;TODO "." and ".." return `true`
(defn sym-link?-old[f]
  (let[canon (if (.getParent f)
                (io/file (-> f .getParentFile .getCanonicalFile) (.getName f))
                f)]
    (not= (.getCanonicalFile canon) (.getAbsoluteFile canon))))

(defn sym-link?[f]
  (java.nio.file.Files/isSymbolicLink (.toPath (io/file f))))


#_(defn get-git-dirs[d]
  (let[all (->> d io/file .listFiles seq)
       dirs (filter #(.isDirectory %) all)
       is-git-dir (mcontains? (map #(.getName %) dirs) ".git")]
    (if is-git-dir
      [(.getCanonicalPath d)]
      (reduce concat (map get-git-dirs dirs)))))

(defn git-repo-clean?[d]
  (p/with-repo d
    (->>
     (p/git-status repo)
     vals
     (apply clojure.set/union)
     empty?)))

;;check if there are local files (.gitignore)
(defn git-repo-untracked[d]
  (p/with-repo d
    (->>
     (p/git-status repo)
     :untracked
     (remove #(-> % clojure.java.io/file sym-link?))
     not-empty)))

(defn rh[a] {:name (.getName (.getValue a)) :leaf (.getName (.getLeaf (.getValue a))) :id (.getName (.getObjectId (.getValue a)))})
(defn git-repo-heads[d]
  (p/with-repo d
    (->>
     repo
     .getRepository
     .getAllRefs
     (map rh)
     doall
     )))

#_(defn ll[x] (prn x) x)
(defn git-repo-touched[d]
  (p/with-repo d
    (->>
     (->
      (p/git-status repo)
      (dissoc :untracked)
      vals)
     (apply clojure.set/union)
     not-empty)))

(defn git-repo-local-commits[d] ;;this is the first version which might not be accurate
  (p/with-repo d
    (let[commit-ref-f (fn[e] [(.getName (.getObjectId e)) (.getName e)])
         commit-ref-groups-f (fn[scope] (group-by first (map commit-ref-f (p/git-branch-list repo scope))))
         keys-f (fn[ps] (-> ps keys set))
         diff-f (fn[d1 d2] (mapcat d1 (clojure.set/difference (keys-f d1) (keys-f d2))))
         local (commit-ref-groups-f :local)
         remote (commit-ref-groups-f :remote)]
      {:extra (diff-f local remote) :missing (diff-f remote local)})))

(comment
  (use 'core :reload)
  (def p "/home/.../code")
  (def a1 (ls-walk git-unclean nil p))
  (def a2f (into {} (for [k (->> a1 (filter (comp empty? second)) (map first))] [k (git-repo-heads k)])))
  (def a2r (into {} (for [k (->> a1 (remove (comp empty? second)) (map first))] [k (git-repo-heads k)])))
  (->> a2f (group-by second) (map second) (map (partial map first)) (sort-by first) pprint)
  (->> a2r (group-by second) (map second) (map (partial map first)) (sort-by first) pprint)
  (->> [[]] (map git-cmd-check) dorun)
  (pprint (ls-unclean-git-dirs p))
)


#_(defn git-ls-remote-repository[d] (->> (-> (Git/lsRemoteRepository) (.setRemote d) (.setHeads true) (.call)) (map #(.getName %))))

#_(defn git-ls-remote[d]
  (p/with-repo d
    (p/git-ls-remote repo)))

(defn git-list-stash[d]
  (p/with-repo d
    (p/git-list-stash repo)))

(defn git-branch-list[d]
  (p/with-repo d
    (p/git-branch-list repo :remote)))

(defn git-get-remote[d]
  (let[config (p/with-repo d (->> repo .getRepository .getConfig))
        remotes (.getSubsections config "remote")]
    (map #(.getString config "remote" % "url") remotes)))

(defn ls-walk[f1 f2 d]
  (let[fd (io/file d)
       all (->> fd .listFiles seq (remove #(#{".neater" ".DS_Store"} (.getName %))) (remove sym-link?))
       [dirs files] ((juxt filter remove) #(.isDirectory %) all)
       is-git-dir (mcontains? (map #(.getName %) dirs) ".git")
       r (if is-git-dir
           (if f1 (f1 d))
           (if f2 (f2 d)))]
    (if is-git-dir
      [(.getPath fd) r]
      (into {} (map (partial ls-walk f1 f2) dirs)))))


(defn git-unclean[d]
  (let [fd (io/file d)
        pd (.getPath fd)]
    (remove nil?
            [(when-not (git-repo-clean? d)
               "is not clean")
             (when-let[ut (git-repo-untracked d)]
               (str "contains " (count ut) " untracked: " (str/join ", " (take 3 ut))))
             (when-let[ut (git-repo-touched d)]
               (str "contains " (count ut) " touched: " (str/join ", " (take 3 ut))))
             (when (empty? (git-get-remote d))
               "has no remotes")
             (when-let[lc (-> d git-repo-local-commits :extra not-empty)]
               (str "branches missmatch: " lc))
             (when-let[ls (-> d git-list-stash vec not-empty)]
               (str "stash is present: " (take 3 ls)))])))

;;use for better output
#_(when-not is-git-dir
    (if (not-empty files)
      (println pd "non Git with local files" #_files)
      (when (empty? dirs)
        (println pd "non empty"))))

(defn ls-unclean-git-dirs[d]
  (ls-walk git-unclean nil d))


(defn git-cmd-check[dd] (println "------") (dorun (map #(println (format "cd %s;git status;git show-ref --heads; git branch --all;git tag;gitk --all&" %)) dd)))


;;sync local files using soft links to gitlocal
;;create a file with state to clone everything from GitHub/recover from a file with a state
;;fetch and check for forced pushes
;;consolidate git functions into a my lib
;; git, gitlocal, gitdb, gitwip, nongit, nongitcopy
;; account for nested git repos
;; add stash detection
