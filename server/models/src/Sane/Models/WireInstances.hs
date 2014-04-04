
instance ProtocolBuffer FullUser W.FullUser where
  proto = iso toWire fromWire
    where
      toWire :: FullUser -> W.FullUser
      toWire u = W.FullUser
        { W.userUsername    = u ^. username . field
        , W.userName        = u ^. name . field
        , W.userEmail       = u ^. email . field
        , W.userCellphone   = u ^. cellphone . field
        , W.userAvatar      = u ^. avatar . field
        , W.userStripeToken = u ^. stripeToken . field
        }

      fromWire :: W.FullUser -> FullUser
      fromWire u = FullUser
        { _fuUsername     = W.userUsername u ^. from field
        , _fuName         = W.userName u ^. from field
        , _fuEmail        = W.userEmail u ^. from field
        , _fuCellphone    = W.userCellphone u ^. from field
        , _fuAvatar       = W.userAvatar u ^. from field
        , _fuStripeToken  = W.userStripeToken u ^. from field
        , _fuFacebookAuth = Nothing
        }

instance ProtocolBuffer Task W.Task where
  proto = iso toWire fromWire
    where
      toWire = W.Task
      fromWire = Task
