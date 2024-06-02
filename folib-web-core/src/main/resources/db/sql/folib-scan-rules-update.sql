alter table scan_rules
    add column bom_on_scan tinyint default 0 null;
comment on column "scan_rules"."bom_on_scan" is 'bom扫描';

alter table scan_rules
    add column project_uuid varchar (255) default '' null;
comment on column "scan_rules"."project_uuid" is '父项目uuid';