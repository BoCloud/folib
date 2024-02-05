ALTER TABLE `dict` MODIFY `alias` text;

INSERT INTO `dict` ( `dict_type`, `dict_key`, `dict_value`, `alias`, `comment` )
VALUES
	( 'folib_upgrade_task', 'method', 'com.veadan.folib.components.common.CommonComponent@handlerRole', '{\"accessModel\":{\"apiAuthorities\":[\"ARTIFACTS_RESOLVE\",\"SEARCH_ARTIFACTS\",\"ARTIFACTS_VIEW\",\"CONFIGURATION_VIEW_METADATA_CONFIGURATION\"],\"storageAuthorities\":[]},\"description\":\"匿名用户角色\",\"name\":\"ANONYMOUS\"}', 'unexecuted' );

INSERT INTO `dict` ( `dict_type`, `dict_key`, `dict_value`, `alias`, `comment` )
VALUES
	( 'folib_upgrade_task', 'method', 'com.veadan.folib.services.ArtifactWebService@dockerLayoutUpgradeAll', '', 'unexecuted' );