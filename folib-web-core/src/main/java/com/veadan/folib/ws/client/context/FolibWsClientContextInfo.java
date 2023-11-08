package com.veadan.folib.ws.client.context;

import com.veadan.folib.ws.common.FolibWsSessionContextHolder;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.springframework.web.socket.WebSocketSession;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/11/8 17:34
 * @since x.x.x
 */
@Data
@Accessors(chain = true)
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class FolibWsClientContextInfo extends FolibWsSessionContextHolder.FolibWsSessionContextInfo<WebSocketSession> {
}
