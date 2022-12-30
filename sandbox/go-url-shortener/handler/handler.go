package handler

import (
	"net/http"

	"github.com/ar90n/lab/sandbox/go-url-sortener/shortener"
	"github.com/ar90n/lab/sandbox/go-url-sortener/store"
	"github.com/gin-gonic/gin"
)

type UrlCreationRequest struct {
	LongUrl string `json:"long_url" binding:"required"`
	UserId  string `json:"user_id" binding:"required"`
}

func CreateShortUrl(c *gin.Context) {
	var creatinoRequest UrlCreationRequest
	if err := c.ShouldBindJSON(&creatinoRequest); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
		return
	}

	shortUrl := shortener.GenerateShortLink(creatinoRequest.LongUrl, creatinoRequest.UserId)
	store.SaveUrlMapping(shortUrl, creatinoRequest.LongUrl, creatinoRequest.UserId)

	host := "http://localhost:9808/"
	c.JSON(200, gin.H{
		"message":   "short url created successfully",
		"short_url": host + shortUrl,
	})
}

func HandleShortUrlRedirect(c *gin.Context) {
	shortUrl := c.Param("shortUrl")
	initialUrl := store.RetrieveInitialUrl(shortUrl)
	c.Redirect(302, initialUrl)
}
