alter table `scan_rules`
    add column `bom_on_scan` tinyint(1) default 0 null comment 'bom扫描' after `on_scan`;

alter table `scan_rules`
    add column `project_uuid` varchar (255) default '' null comment '父项目uuid' after `bom_on_scan`;