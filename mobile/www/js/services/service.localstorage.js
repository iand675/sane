sane.factory('localStorageService', [function () {
	var store = new Lawnchair({name: 'localStorageService', adaptor:'dom'}, function () {});

	function set(key, object) {
		store.save({key: key, value: object});
	}

	function get(key) {
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
		get: get,
		set: set,
		remove: remove
	};
}]);