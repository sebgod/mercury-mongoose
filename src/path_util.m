%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: path_util.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Fri Dec 26 22:10:56 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% Mongoose URI helper.
%----------------------------------------------------------------------------%

:- module mercury_mongoose.path_util.

:- interface.

:- import_module list.
:- import_module string.

%----------------------------------------------------------------------------%

:- type encoded_uri.

:- func decode_uri(encoded_uri) = string.

%----------------------------------------------------------------------------%

:- type uri_encoded_path.

    % split_path(Path) = Components:
    % NOTE: Leading slash will yield an empty string.
    %
:- func split_and_decode_path(uri_encoded_path) = list(string).

:- func to_encoded_string(uri_encoded_path) = string.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%----------------------------------------------------------------------------%

:- type encoded_uri == string.

:- type uri_encoded_path == string.

%----------------------------------------------------------------------------%

    % TODO: Optimise for C to avoid excessive memory allocations
split_and_decode_path(Path) = map(decode_uri, split_at_char('/', Path)).

%----------------------------------------------------------------------------%

decode_uri(_) = sorry($file, $pred, $grade ++ " does not support " ++ $pred).

:- pragma foreign_proc("C", decode_uri(EncodedUri::in) = (DecodedUri::out),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    int n = (int) strlen(EncodedUri);
    MR_allocate_aligned_string_msg(DecodedUri, n, MR_ALLOC_ID);
    DecodedUri[n] = '\\0';
    mg_url_decode(EncodedUri, n, DecodedUri, n + 1, 0);
").

%----------------------------------------------------------------------------%

to_encoded_string(Uri) = Uri.

%----------------------------------------------------------------------------%
:- end_module mercury_mongoose.path_util.
%----------------------------------------------------------------------------%
