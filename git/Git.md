
# Git Workflow: Branches, Tags, PRs, Rollback

## Komandna linija

- **Create feature branch** (work isolated from `master`):

  - `git checkout -b feat/naslovna-static-order`
  - `git add . && git commit -m "Naslovna: statički redosled 2025→2022"`
  - `git push -u origin feat/naslovna-static-order`

- **Tag a stable state** (easy return point):
  
  - `git tag v1.3-naslovna-static`
  - `git push origin v1.3-naslovna-static`

- **Open a Pull Request (PR)**:

  - PR je zahtev da se promene sa tvoje grane spoje u `master`.
  - Otvara se na GitHub-u: izaberi granu `feat/naslovna-static-order` → "Compare & pull request" → potvrdi.
  - Prednosti: pregled, diskusija, automatski merge; lako odustajanje.

- **Revert na tag (hard reset)**:

  - `git checkout master`
  - `git reset --hard v1.3-naslovna-static`
  - `git push -f origin master` (oprez: menja istoriju; koristi ako si jedini na repou)

- **Revert pojedinačnog commita (bez menjanja istorije)**:

  - Nađi SHA: `git log --oneline`
  - `git revert <SHA>`
  - `git push`

- **Nova grana za dinamički kod**:
  
  - `git checkout -b feat/naslovna-dynamic`
  - Radi i komituj promene; gurni: `git push -u origin feat/naslovna-dynamic`
  - Otvori PR → pregled → merge kada je spremno.

### Brze napomene

- `feat/...` grane su za nove funkcionalnosti; `fix/...` za ispravke; `chore/...` za održavanje.
- Taguj samo stabilna stanja (release/rollback tačke).
- PR je preporučen put za spajanje u `master`; direktni push u `master` izbegavati.

## Code okolina

- **Branch**: napravi novu granu u VS Code

  - **Status bar** : → klik na naziv grane → Create new branch → upiši ime → Publish branch.
  - **Commit/Push**: kroz Source Control

- **Označi izmene** : → “+” (Stage) → poruka → Commit → “Sync” ili “Push”.

- **Tag** : kreiraj i pošalji tag

- **Command Palette (Ctrl+Shift+P)** : → “Git: Create Tag” → izaberi HEAD → upiši ime → “Git: Push Tags”.

- **Pull Request (PR)** : uz GitHub ekstenziju

- **Command Palette** : → “GitHub: Create Pull Request” → izaberi source branch i target master → potvrdi.

  - **Pregledaj i spoji PR** : u VS Code (“Pull Requests” panel).

- **Revert commit** : (bez menjanja istorije)
  - **Source Control** → “Open Timeline” → izaberi commit → “Revert Commit” → Push.
  - **Reset na tag/commit** (uz GitLens, oprez)

- **GitLens Commits** : → desni klik na tag/commit → “Reset current branch to here (hard)” → Force Push ako traži.
  Koristi samo ako si jedini na repou.

### Preporučene ekstenzije

- GitLens — Git supercharged
- GitHub Pull Requests and Issues

### Preporučeni tok rada

- **Feature grana** : (npr. feat/naslovna-static-order) → Commit/Push → Tag stabilnog stanja → PR u master.
- **Za eksperimentalne promene** : odvojena grana (npr. feat/naslovna-dynamic) + PR; lako odustajanje ako ne valja.
