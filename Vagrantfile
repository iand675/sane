# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  # All Vagrant configuration is done here. The most common configuration
  # options are documented and commented below. For a complete reference,
  # please see the online documentation at vagrantup.com.

  # Every Vagrant virtual environment requires a box to build off of.
  config.vm.box = "default"

  # The url from where the 'config.vm.box' box will be fetched if it
  # doesn't already exist on the user's system.
  config.vm.provider :vmware_fusion do |v, override|
    override.vm.box_url = "http://files.vagrantup.com/precise64_vmware.box"
    v.vmx['memsize'] = '2048'
    v.vmx['numvcpus'] = '2'
  end

	config.vm.provider :virtualbox do |v, override|
    override.vm.box_url = "http://files.vagrantup.com/precise64.box"
		v.memory = 2048
	end

	config.vm.provision 'ansible' do |a|
		a.playbook = 'playbook/site.yml'
		a.inventory_path = 'playbook/development'
	end
  # config.vm.provision :ansible do |ansible|
  #   ansible.playbook = "playbook/site.yml"
  #   ansible.inventory_path = "playbook/development"
  # end
  # Create a forwarded port mapping which allows access to a specific port
  # within the machine from a port on the host machine. In the example below,
  # accessing "localhost:8080" will access port 80 on the guest machine.
  # config.vm.network :forwarded_port, guest: 80, host: 8080

  # Create a private network, which allows host-only access to the machine
  # using a specific IP.
  config.vm.network :private_network, ip: "192.168.33.10"

  # Create a public network, which generally matched to bridged network.
  # Bridged networks make the machine appear as another physical device on
  # your network.
  # config.vm.network :public_network

  # If true, then any SSH connections made will enable agent forwarding.
  # Default value: false
  # config.ssh.forward_agent = true

  # Share an additional folder to the guest VM. The first argument is
  # the path on the host to the actual folder. The second argument is
  # the path on the guest to mount the folder. And the optional third
  # argument is a set of non-required options.
  # config.vm.synced_folder "../data", "/vagrant_data"
end
