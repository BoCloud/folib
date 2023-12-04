
<template>
	<a-card :bordered="false" :bodyStyle="{padding: 0,}">
		
	</a-card>
</template>

<script>
import { ACCESS_TOKEN, USER_INFO } from '@/store/mutation-types'
import store from '@/store'
import { ssoLogin,
} from '@/api/sso'
const key = 'login'
export default ({
  created() {
		this.init()
  },
  data() {
    return {}
  },
  methods: {
		message(type, message) {
      if (!message) {
        message = "操作成功"
      }
      this.$notification[type]({
        message: message,
        description: "",
      })
    },
		openLoading() {
			this.$message.config({top: `80px`})
			this.$message.loading({ content: '登录中，请稍等...', key, duration: 30})
		},
		closeLoading() {
			this.$message.loading({ content: '登录中，请稍等...', key, duration: 0.01})
		},
		init() {
			this.openLoading()
			const token = this.$route.query.token
			const idToken = this.$route.query.idToken
			if (!token) {
				this.closeLoading()
				this.message("error", "缺少参数")
				return false
			}
			if (idToken) {
				localStorage.setItem("SSOIdToken",idToken)
			}
			store.dispatch("Token", token)
			store.dispatch("GetInfo").then((resp) => {
				this.$router.push({ name: 'storages' })
				setTimeout(() => {
					this.message("success","欢迎")
				}, 100)
			}).finally(() => {
				this.closeLoading()
			})
		}
  }
})

</script>

<style lang="scss">

</style>

