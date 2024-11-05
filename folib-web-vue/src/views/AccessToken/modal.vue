<template>
  <div class="tokenModal">
    <a-modal
        :title="($t('AccessToken.AddToken'))"
        :visible="visible"
        @ok="confirmModal"
        @cancel="handleCancel"
    >
      <!-- 令牌描述 -->
      <a-form-item :label="$t('AccessToken.Description')">
        <a-textarea :placeholder="$t('AccessToken.DescriptionPlaceholder')" v-model="form.description" show-count
                    :maxLength="50"/>
      </a-form-item>
      <!-- 用户名 -->
      <a-form-item :label="$t('AccessToken.Username')" required>
        <a-select
            style="width: 100%"
            v-model="form.username"
            :placeholder="$t('AccessToken.UsernamePlaceholder')"
            showSearch
            allowClear
            :options="users"
        />
      </a-form-item>
      <!-- 过期时间 -->
      <a-form-item :label="$t('AccessToken.ExpireTime')" required>
        <a-select v-model="form.expire" :placeholder="$t('AccessToken.ExpireTimePlaceholder')" style="width: 100%;">
          <a-select-option value="1">{{ $t('AccessToken.SevenDay') }}</a-select-option>
          <a-select-option value="2">{{ $t('AccessToken.ThirtyDay') }}</a-select-option>
          <a-select-option value="3">{{ $t('AccessToken.NinetyDay') }}</a-select-option>
          <a-select-option value="4">{{ $t('AccessToken.OneYear') }}</a-select-option>
          <a-select-option value="0">{{ $t('AccessToken.Never') }}</a-select-option>
        </a-select>
      </a-form-item>
    </a-modal>
    <CopyModal :tokenInfo="tokenInfo" ref="modal"></CopyModal>
  </div>
</template>

<script>

import {getUsers} from "@/api/users";
import {genToken} from "@/api/accessToken";
import CopyModal from "./CopyModal.vue"


export default {
  name: "modal",
  props:{},
  components:{
    CopyModal
  },
  data() {
    return {
      visible: false,
      form: {},
      users: [],
      tokenInfo: {}
    }
  },
  methods: {
    confirmModal() {
      genToken(this.form).then(res => {
        this.tokenInfo = res;
        this.visible = false;
        this.form={};
        this.$refs.modal.openModal();
        this.$emit('refreshData');
      }).catch(err => {
        this.$message.error("令牌创建失败！");
      });
    },
    handleCancel() {
      this.visible = false;
      this.form={};
    },
    openModal() {
      this.visible = true;
    },
    getUsers() {
      getUsers().then(res => {
        this.users = res.users.filter(user => user.enabled).map(item => ({
          label: item.username,
          value: item.username
        }));
      })
    },
  },
  mounted() {
    this.getUsers();
  }
}
</script>

<style scoped>

</style>