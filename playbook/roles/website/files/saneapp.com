server {
	listen   8001;

	root   /var/www/site;
	index  index.html;

  access_log off;
  expires max;
}
