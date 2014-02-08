sane.services.factory('localStorageService', [function () {
	var store = new Lawnchair({name: 'localStorageService', adaptor:'dom'}, function () {});

	function setItem(key, object) {
		store.save({key: key, value: object});
	}

	function getItem(key) {
		var record;

		store.get(key, function(item) {
			if (item)
				record = item.value;
		});

		return record;
	}

	function remove(key) {
		store.remove(key);
	}

	return {
		getItem: getItem,
		setItem: setItem,
		remove: remove
	};
}]);
