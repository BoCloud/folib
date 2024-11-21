package com.veadan.folib.domain.migrate;

/**
 * @author huayanjun
 * @since 2024-11-20 15:19
 */
public class MigrateInfo {

    // 状态 1-成功 2-失败
    private int status;

    private int totalUser;

    private int sucUser;

    private int failUser;

    private int group;
    private int repository;
}
