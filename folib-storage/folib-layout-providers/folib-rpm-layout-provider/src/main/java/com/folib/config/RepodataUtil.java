/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.File;
import java.io.IOException;

/**
 * 调用createrepo 命令
 */
@Component
@Slf4j
public class RepodataUtil {

    /**
     * 创建本地rpm  仓的元数据信息
     *
     * @param fileAbsolutePath 生成 repo元数据的全路径
     */
    public void createRepo(String fileAbsolutePath) throws IOException, InterruptedException {
            Runtime runtime = Runtime.getRuntime();
            String command = "createrepo " + fileAbsolutePath;
            Process pro = runtime.exec(command);
            int status = pro.waitFor();
            if (status != 0) {
                log.error("Failed to call shell's command ");
            }
    }

    /**
     * 更新Rpm 仓库的索引
     *
     * @param fileAbsolutePath 生成 repo元数据的全路径
     * @throws IOException io 异常
     * @throws InterruptedException 中断的异常
     */
    public void updateIndex(String fileAbsolutePath) throws IOException, InterruptedException {
        Runtime runtime = Runtime.getRuntime();
        String command = "createrepo --update " + fileAbsolutePath;
        Process pro = runtime.exec(command);
        int status = pro.waitFor();
        if (status != 0) {
            log.error("Failed to call shell's command ");
        }
        log.info("Rpm index updated");
    }

    /**
     * 删除本地rpm 仓的元数据文件夹
     *
     * @param repodataPath 生成 repo元数据的全路径
     */
    public void deleteRepo(String repodataPath) {
        File file = new File(repodataPath);
        if(file.exists()&&file.isDirectory()){
            file.delete();
        }
    }

}
