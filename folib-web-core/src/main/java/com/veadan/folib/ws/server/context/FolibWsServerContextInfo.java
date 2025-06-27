package com.veadan.folib.ws.server.context;

import com.veadan.folib.ws.common.FolibWsSessionContextHolder;
import com.veadan.folib.ws.server.manage.FolibWsServerRunManage;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @author veadan
 * @date 2023/11/8 17:34
 */
@Data
@Accessors(chain = true)
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class FolibWsServerContextInfo extends FolibWsSessionContextHolder.FolibWsSessionContextInfo<FolibWsServerRunManage.FolibWsClientRun> {
    private String nodeName;
}
