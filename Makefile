src_dir = ./vendor/sagemaker-python-sdk/src/sagemaker/image_uri_config
dest_dir = ./inst

get-image-config:
	@git submodule init
	@git submodule update --remote
	@mkdir -p $(dest_dir)
	@cp -R $(src_dir) $(dest_dir)

