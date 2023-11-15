package com.veadan.folib.ws.server.context;

import com.veadan.folib.ws.common.FolibWsSessionContextHolder;
import com.veadan.folib.ws.server.manage.FolibWsServerRunManage;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/11/8 17:34
 * @since x.x.x
 */
@Data
@Accessors(chain = true)
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class FolibWsServerContextInfo extends FolibWsSessionContextHolder.FolibWsSessionContextInfo<FolibWsServerRunManage.FolibWsClientRun> {
    private String nodeName;
}
