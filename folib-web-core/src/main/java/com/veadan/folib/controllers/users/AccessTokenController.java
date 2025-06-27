package com.veadan.folib.controllers.users;

import com.github.pagehelper.PageInfo;
import com.veadan.folib.entity.AccessToken;
import com.veadan.folib.dto.accesstoken.AccessTokenDto;
import com.veadan.folib.dto.accesstoken.AccessTokenResponse;
import com.veadan.folib.services.AccessTokenService;
import io.swagger.annotations.Api;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.util.List;

/**
 * @author veadan
 * @since 2024-08-20 11:17
 */
@RestController
@RequestMapping("/api/accessToken")
@Api(tags = "访问令牌")
@PreAuthorize("hasAuthority('ADMIN')")
public class AccessTokenController {

    @Resource
    private AccessTokenService accessTokenService;


    @PostMapping
    public ResponseEntity<AccessTokenResponse> generateToken(@RequestBody @Validated AccessTokenDto form) {
        AccessTokenResponse response = accessTokenService.generate(form);
        return ResponseEntity.ok(response);
    }

    @GetMapping
    public ResponseEntity<PageInfo<AccessToken>> list(Integer pageSize, Integer pageNum, @RequestParam(required = false) String tokenId) {
        List<AccessToken> tokens = accessTokenService.list(pageSize, pageNum, tokenId);
        return ResponseEntity.ok(PageInfo.of(tokens));
    }

    @DeleteMapping
    public ResponseEntity<String> delete(Long id,String tokenId){
        accessTokenService.delete(id,tokenId);
        return ResponseEntity.ok("revoked success");
    }


}
