package com.veadan.folib.ws.client.context;

import com.veadan.folib.ws.client.manage.FolibWsClientRunManage;
import com.veadan.folib.ws.common.FolibWsSessionContextHolder;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

/**
 * @author veadan
 * @date 2023/11/8 17:34
 */
@Data
@Accessors(chain = true)
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class FolibWsClientContextInfo extends FolibWsSessionContextHolder.FolibWsSessionContextInfo<FolibWsClientRunManage.FolibWsServerRun> {
}
