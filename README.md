<!--
SPDX-FileCopyrightText: 2025 Łukasz Niemier <#@hauleth.dev>

SPDX-License-Identifier: MIT
-->

# `os_process`

Missing piece of system information in OTP.

This is small library that allows users to fetch more information about the
current process from the OS. ERTS support only one information - process ID
(that is strangely returned as a charlist).

This library exposes more info about process:

- process ID (as integer)
- priority
- effective/real group/user ID (on Unixes)
- user name (on Windows)

In addition to that, there is small helper function that also return C type
sizes.
