# Contributors

`outbreakTools` is developed and maintained by Türkiye FETP (Field Epidemiology
Training Programme) members.

## Lead authors

| Name | Role | Contact |
|---|---|---|
| **Gülser Doğan Türkçelik** | Author & maintainer — lead epidemiologist, statistical methods | gulser.dogan@fetp.gov.tr |
| **Muammer Beslen**         | Author — module architecture, R/jamovi integration            | muammer.beslen@fetp.gov.tr |

## Acknowledgements

We thank the wider Türkiye FETP community for outbreak investigation
experience that shaped the analytical workflow, and the jamovi
development team (Jonathon Love, Damian Dropmann, Ravi Selker) for
providing an excellent open platform for field-friendly statistics.

Statistical methods follow the conventions of:
- Rothman KJ, Greenland S, Lash TL (2008) *Modern Epidemiology*, 3rd ed.
- Schlesselman JJ (1982) *Case-Control Studies*. Oxford University Press.
- CDC (2012) *Principles of Epidemiology in Public Health Practice*, 3rd ed.

The OpenRefine-style clustering implemented in v1.1.0 is inspired by the
[OpenRefine](https://openrefine.org/) project — original algorithm by
David Huynh and the OpenRefine team.

## How to contribute

Pull requests are welcome. Before opening one:

1. **Open an issue first** describing what you plan to change. This avoids
   wasted effort if the change conflicts with planned development.
2. Follow the existing code style: snake_case for internal helpers, prefix
   non-exported utilities with `.obt_`, document new helpers with roxygen
   comments.
3. Add a brief note to the change in commit message — what changed, why.
4. If you add a new dependency, justify it in the PR description and add it
   to `DESCRIPTION` under `Imports:`.

Bug reports, feature requests, and questions go to the
[Issues](https://github.com/turkiye-fetp/outbreakTools/issues) tab.

## License

All contributions are released under the same GNU General Public License v3.0
(or later) that covers the rest of the module — see [LICENSE](LICENSE).
By submitting a pull request you agree your contribution is licensed under
GPL-3.0-or-later.
