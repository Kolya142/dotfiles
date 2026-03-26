git config set --global user.name "Gudhein3"
git config set --global user.email "nikolasshevelkos@gmail.com"
# It's a publicly avaliable identifier of my key. You cannot hack me with it, lol.
KEY=6E344F6A15BD2DCC354CD4042BB740FB6D4AE523
gpg --list-secret-keys | grep $KEY && git config set --global user.signingkey $KEY || echo "You haven't the PGP key"
git config --global commit.gpgsign true
