# Updating

This document explains how to update package between major versions
of the package.

0.5.x -> 0.6
============

1. Sender, Recipient types were changed from 'Data.Text.Text' to
[Network.Mail.Mime.Address](http://hackage.haskell.org/package/mime-mail-0.5.0/docs/Network-Mail-Mime.html#t:Address).
   This change allows better interoperability with 'mime-mail' package, in addition
   it solved a problem with specifying sender address in the API.
   As Address type implements 'IsString' type class then just having '-XOverloadedStrings'
   extension will be enough. However you should note, that "FirstName LastName <email>"
   will be parsed as `Address Nothing "FirstName LastName <email>"` and not as
   `Address (Just "FirstName LastName") "email"`, it may be surprising but that functionality
   didn't work with older HaskellNet, so it was not changed.
2. Exception. Previously the package exposed only `IOException` custom exceptions were send
   using `failure` (`UserException` constructor). Now there is `SMTPException` family, so
   if you used to capture SMTP exceptions by processing `IOExceptions` you should capture
   `SMTPException` now. The package may still expose `IOException` in case of a network
   failure.
3. `Subject` type is now using Text. Enabling `-XOverladedStrings` extension should help
   in this case.
4. All mail sending functions were deprecated instead of them there is the 'sendMail'
   function. Documentation and deprecation warning for each function explains how to change
   the code to make it work.
5. Now all `doSmtp*` functions implements graceful close, and there is a new `gracefullyCloseSMTP`
   function to run graceful close, you may consider switching to that function from `closeSTMP`.
   However it should be done with care as `gracefullyCloseSTMP` can be run only on the
   connection that is in a awaiting command from the user state.